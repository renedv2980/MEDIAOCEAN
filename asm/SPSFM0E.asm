*          DATA SET SPSFM0E    AT LEVEL 033 AS OF 08/11/11                      
*PHASE T2170EA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2170E - SIR NSID RECORD MAINTENANCE                  *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*               T2171F (NSID/DETAIL MINI-CONTROLLER) VIA T21700       *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, COPY, LIST             *         
*                                                                     *         
*  INPUTS       SCREEN T217BE (NSID MAINTENANCE)                      *         
*               SCREEN T217AE (NSID LIST)                             *         
*                                                                     *         
*  OUTPUTS      UPDATED NSID AND DETAIL RECORDS                       *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- FOURTH BASE                                     *         
*               R7 -- THIRD BASE                                      *         
*               R8 -- SECOND BASE                                     *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - NSID RECORD                                     *         
*               IO2 - DETAIL RECORD                                   *         
*               IO3 - MISC.                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2170E  SIR RECORDS - STATION INVENTORY LIST'                   
***********************************************************************         
*                                                                     *         
*  COMMENTS                                                           *         
*                                                                     *         
*  THERE IS A STRICT ONE-TO-ONE CORRESPONDENCE BETWEEN EACH NSID      *         
*  DAY/TIME ELEMENT AND ITS CHILD DETAIL RECORD.  EVERY TIME A        *         
*  NSID RECORD IS ADDED OR CHANGED, THIS IS REFLECTED BY THE          *         
*  ADDITION, DELETION, OR UPDATING OF THE ASSOCIATED DETAIL RECORDS.  *         
*                                                                     *         
*  THE MAINTENANCE ACTIONS FOR THIS PHASE ARE CALLED BY GENCON IN A   *         
*  NON-STANDARD FASHION.  ONLY TWO MODES ARE USED, VALKEY AND VALREC. *         
*  FOR ALL ACTIONS EXCEPT CHANGE, GENCON CALLS WITH VALKEY FOLLOWED   *         
*  IMMEDIATELY BY VALREC.  IN THE CHANGE ACTION, VALKEY IS PASSED     *         
*  ONLY IF THE KEY HAS CHANGED.                                       *         
*                                                                     *         
*  WHEN A SELECT IS MADE ON THE MAINTENANCE SCREEN, THE APPROPRIATE   *         
*  MAINTENANCE SCREEN IS LOADED IN, AND THE SELECTED DETAIL RECORD    *         
*  IS DISPLAYED.  ON THE NEXT TRANSACTION, CONTROL IS DIRECTED BY     *         
*  THE MINI-CONTROLLER T2171F, WHICH WILL LOAD THIS PHASE AND SCREEN  *         
*  BACK IN UPON COMPLETION OF A SELECT.                               *         
*                                                                     *         
*  WHEN THIS PHASE IS LOADED BY T2171F, GENCON IS OUT OF THE          *         
*  PICTURE, BECAUSE IT HAS BEEN CALLED IN SLAVE MODE.  THIS MEANS,    *         
*  AMONG OTHER THINGS, THAT THE RESPONSIBILITY FOR SAVING SYSD AND    *         
*  HANDLING ERRORS RESTS WITH THE APPLICATION.                        *         
*                                                                     *         
*  A MAJOR CAVEAT -- THIS PHASE SHARES THE 'SYSSPARE' REGION OF SYSD  *         
*  WITH THE PHASES WHICH IT LOADS.  THEREFORE, THE FIRST 1024 BYTES   *         
*  OF SYSSPARE ARE RESERVED FOR THIS PHASE, WHILE THE REMAINDER IS    *         
*  USED BY THE LOADED PHASE.  IF MORE THAN 1024 BYTES IN SYSSPARE ARE *         
*  NEEDED BY THIS PHASE, THEN THE LOADED PHASES MUST BE CHANGED.      *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T2170E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2170E**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING T2170E,RB,R8,R7,R6                                               
         ST    RD,SAVERD                                                        
         ST    R3,RELO                                                          
*                                                                               
         CLC   =C'SEL',CONACT      TEST ACTION SELECT                           
         BNE   MAIN10              NO                                           
         CLC   SIRSCRN,=C'BE'      YES - TEST MAINT. SCREEN LOADED              
         BE    *+16                                                             
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         MVI   ACTNUM,ACTSEL                                                    
         BE    DR                                                               
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VK                                                            
         B     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   XIT                                                              
         CLI   ACTNUM,ACTADD                                                    
         BE    AD                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    DR                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    DR                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BE    COPY                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    LR                                                               
*                                                                               
         CLI   ACTNUM,ACTDEL       CAN'T DELETE THESE RECORDS                   
         BNE   *+16                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
         CLI   ACTNUM,ACTREST      THEREFORE, CAN'T RESTORE THEM EITHER         
         BNE   *+16                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       NTR1                                                                   
*                                                                               
         CLI   RESTART,C'Y'        TEST REDISPLAY FROM BEGINNING                
         BE    VK10                                                             
*                                                                               
         MVI   NEWKEY,C'N'         ASSUME KEY IS UNCHANGED                      
*                                                                               
         TM    SIRSTAH+4,FINPVAL   STATION WAS CHANGED?                         
         BZ    VK10                YES                                          
         TM    SIRDPTH+4,FINPVAL   DAYPART                                      
         BZ    VK10                YES                                          
         TM    SIRPERH+4,FINPVAL   PERIOD                                       
         BZ    VK10                YES                                          
         TM    SIRSCHH+4,FINPVAL   SCHEME                                       
         BZ    VK10                YES                                          
         TM    SIRYRH+4,FINPVAL    YEAR                                         
         BZ    VK10                YES                                          
         CLI   ACTNUM,ACTLIST      PROGTYPE IS ABSENT FOR LIST                  
         BE    *+12                YES                                          
         TM    SIRPRGH+4,FINPVAL   PROGTYPE                                     
         BZ    VK10                NO                                           
*                                                                               
         OC    SVKEY,SVKEY         IS THERE A SAVED KEY?                        
         BNZ   VK12                YES -- USE IT                                
*                                                                               
VK10     MVI   NEWKEY,C'Y'         KEY HAS CHANGED                              
*                                                                               
VK12     CLI   ACTNUM,ACTADD       TEST ACTION ADD                              
         BE    *+16                                                             
         CLI   NEWKEY,C'Y'         TEST KEY HAS CHANGED                         
         BE    VK15                                                             
         B     VKX                                                              
*                                                                               
         CLI   NEWKEY,C'Y'         TEST KEY HAS CHANGED                         
         BE    *+12                                                             
         CLI   FROMADD,C'Y'        TEST DID ADD LAST TIME                       
         BE    VKX                                                              
*                                                                               
VK15     MVI   RESTART,C'N'        BEGINNING A NEW LIST                         
         MVC   OLDKEY,SVKEY        HANG ON TO PREVIOUS KEY                      
         LA    R4,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         USING SIRKEY,R4                                                        
         MVI   SIRKTYPE,SIRKTYPQ   SIR RECORD TYPE                              
*                                                                               
         LA    R2,=X'0900000000010000E3'                                        
         GOTO1 VALIMED             FAKE THE MEDIA FIELD TO 'T'                  
         MVC   SIRKAM,BAGYMD                                                    
*                                                                               
         LA    R2,SIRSCHH          SCHEME                                       
         CLI   SIRSCHH+5,0                                                      
         BNE   VK21                SCHEME WAS GIVEN                             
*                                                                               
         XC    WORK,WORK           READ SID PROFILE FOR AGENCY/MEDIA            
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    VK20                NO -- NO SCHEME IS OK                        
         CLI   WORK+2,C'N'         DEFAULT ALLOWED?                             
         BNE   VK20                YES                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK20     MVC   SIRSCH,=C'ALL'                                                   
         OI    SIRSCHH+6,FOUTTRN   XMIT                                         
         B     VK22                                                             
*                                                                               
VK21     CLC   =C'ALL',SIRSCH      TEST SCHEME FOR ENTIRE AGENCY                
         BE    VK22                                                             
         OC    SIRSCH,=C'   '                                                   
         GOTO1 CLPACK,DMCB,SIRSCH,SIRKCODE                                      
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BE    VK22                YES                                          
         MVI   ERROR,INVSCH                                                     
         B     TRAPERR                                                          
*                                                                               
VK22     MVI   SVUSECMP,C'N'       ASSUME USER DOESN'T USE COMPETITION          
         MVI   SVUSEREP,C'N'       ASSUME USER DOESN'T USE REP MARKETS          
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         CLC   SIRSCH,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   WORK+23(3),SIRSCH                                                
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    *+16                NO                                           
         MVC   SVUSECMP,WORK       SAVE PROFILE VALUES                          
         MVC   SVUSEREP,WORK+1                                                  
*                                                                               
         XC    BMKTSTA,BMKTSTA     MARKET/STATION                               
         LA    R2,SIRSTAH                                                       
         CLI   SIRSTAH+5,0                                                      
         BNE   VK25                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK40                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK25     TM    SIRSTAH+4,FINPNUM   IS IT NUMERIC (MKT NUMBER)                   
         BZ    VK30                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+12                                                             
         MVI   ERROR,INVSTAT                                                    
         B     TRAPERR                                                          
*                                                                               
         CLI   SVUSEREP,C'Y'       TEST READING REP MARKETS                     
         BNE   VK26                NO                                           
         GOTO1 VALIRMKT            YES                                          
         B     VK27                                                             
VK26     GOTO1 VALIMKT             IT'S A MARKET NUMBER                         
*                                                                               
VK27     PACK  DUB,QMKT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,3,SIRKMKT        SAVE THE MARKET                              
         B     VK40                                                             
*                                                                               
VK30     CLI   SVUSEREP,C'Y'       TEST READING REP MARKETS                     
         BNE   VK35                NO                                           
         GOTO1 VALIRSTA            YES                                          
         B     VK37                                                             
VK35     GOTO1 VALISTA             IT'S A STATION                               
*                                                                               
VK37     MVC   SIRKMS,BMKTSTA                                                   
*                                                                               
VK40     MVI   DAYPART,0           DAYPART                                      
         LA    R2,SIRDPTH                                                       
         CLI   SIRDPTH+5,0                                                      
         BNE   VK50                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK60                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK50     MVC   DAYPART,SIRDPT                                                   
         MVC   SIRKDPT,DAYPART                                                  
*                                                                               
VK60     LA    R2,SIRYRH           YEAR                                         
         CLI   SIRYRH+5,0                                                       
         BE    VK70                                                             
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(SIRYRH+5,SIRYR),('PVINSGLO',WORK)                   
*                                                                               
         CLI   DMCB+4,PVRCOK       YEAR WAS OK?                                 
         BE    *+12                                                             
         MVI   ERROR,INVYEAR                                                    
         B     TRAPERR             NO                                           
*                                                                               
         LA    RF,WORK             PERVAL OUTPUT AREA                           
         MVC   SIRKYEAR,PVALBSTA-PERVALD(RF)  BINARY YEAR                       
         XI    SIRKYEAR,X'FF'      YEAR IN ONE'S COMPLEMENT FORM                
*                                                                               
VK70     MVI   PROGTYP,0           PROGRAM TYPE                                 
         CLI   ACTNUM,ACTLIST      FIELD ISN'T ON LIST SCREEN                   
         BE    VK80                                                             
*                                                                               
         CLI   SIRPRGH+5,0                                                      
         BE    VK80                                                             
         CLI   ACTNUM,ACTADD       TEST ACTION ADD                              
         BE    NOPGHERE            YES - NO PROGRAM TYPE ALLOWED                
         MVC   PROGTYP,SIRPRG                                                   
*                                                                               
VK80     LA    R4,KEY              CREATE THE SCHEME KEY                        
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         XC    SIRKMS(9),SIRKMS                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST SCHEME EXISTS                           
         BE    *+16                                                             
         LA    R2,SIRSCHH                                                       
         MVI   ERROR,NOSCHM                                                     
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET THE SCHEME RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EDYCODEQ     LOOK FOR DEFAULT YEAR ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDYELEM,R5                                                       
         MVC   CURYEAR,EDYYEAR     GET THE YEAR                                 
         DROP  R5                                                               
*                                                                               
         LA    R4,SVKEY                                                         
         CLI   SIRKYEAR,0          TEST YEAR IS ABSENT                          
         BNE   VK100               NO, WE HAVE ONE                              
         MVC   SIRKYEAR,CURYEAR    USE THE CURRENT YEAR                         
         MVC   FULL(1),CURYEAR                                                  
         MVC   FULL+1(2),=X'0101'  PRETEND IT'S JAN01                           
         GOTO1 DATCON,DMCB,(3,FULL),(20,DUB)                                    
         MVC   SIRYR,DUB+2         PRINTABLE YY                                 
         OI    SIRYRH+6,FOUTTRN    XMIT THE CURRENT YEAR                        
         XI    SIRKYEAR,X'FF'      YEAR IN ONE'S COMPLEMENT FORM                
         B     VK110                                                            
*                                                                               
VK100    ZIC   R1,SIRKYEAR         COMPARE GIVEN YEAR TO CURRENT YEAR           
         X     R1,=F'255'          MAKE YEAR POSITIVE                           
         ZIC   R0,CURYEAR                                                       
         SR    R1,R0                                                            
         LPR   R1,R1               MUST BE WITHIN ONE YEAR OF CURRENT           
         CH    R1,=H'1'                                                         
         BNH   VK110                                                            
*                                                                               
         LA    R2,SIRYRH           PUT CURSOR ON YEAR FIELD                     
         MVI   ERROR,INVYEAR                                                    
         B     TRAPERR                                                          
*                                                                               
VK110    MVC   YEAR,SIRKYEAR       HANG ON TO YEAR                              
         CLI   DAYPART,0           TEST NO DAYPART GIVEN                        
         BE    VK125                                                            
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EDCCODEQ     LOOK FOR DAYPART CODES                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF DAYPARTS                         
         LA    R5,2(R5)            FIRST DAYPART                                
*                                                                               
VK120    CLC   DAYPART,0(R5)       LOOK FOR THE GIVEN DAYPART                   
         BE    VK125                                                            
         LA    R5,8(R5)            NEXT DAYPART                                 
         BCT   R1,VK120                                                         
         LA    R2,SIRDPTH          PUT CURSOR ON DAYPART FIELD                  
         MVI   ERROR,INVDPT                                                     
         B     TRAPERR                                                          
*                                                                               
VK125    LA    R3,PRGTYPSV         SAVE PROGTYP CODE/NAME IN PRGTYPSV           
         XC    PRGTYPSV,PRGTYPSV                                                
         L     R5,AIO                                                           
         MVI   ELCODE,EPCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VK140               PROGTYP IS OPTIONAL                          
*                                                                               
         USING EPCELEM,R5                                                       
         SR    R0,R0                                                            
         ZIC   R1,EPCLEN                                                        
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            LEAVES NO. OF PROGTYPS IN R1                 
*                                                                               
VK130    MVC   0(8,R3),EPCDCODE    CODE+NAME                                    
         LA    R5,8(R5)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,VK130                                                         
         DROP  R5                                                               
*                                                                               
VK140    CLI   ACTNUM,ACTLIST      FIELD ISN'T ON LIST SCREEN                   
         BE    VK170                                                            
         XC    SIRDPRG,SIRDPRG     CLEAR THE PROGRAM TYPE DESCRIPTION           
         OI    SIRDPRGH+6,FOUTTRN  XMIT                                         
         CLI   PROGTYP,0           TEST ANY PROGRAM TYPE FILTER                 
         BE    VK170                                                            
*                                                                               
         LA    R3,PRGTYPSV                                                      
VK150    CLC   PROGTYP,0(R3)       LOOK FOR THE PROGRAM TYPE                    
         BE    VK160                                                            
         LA    R3,8(R3)            NEXT ENTRY                                   
         CLI   0(R3),0             TEST END OF TABLE                            
         BNE   VK150               NO                                           
*                                                                               
         LA    R2,SIRPRGH          PROGRAM TYPE IS INVALID                      
         MVI   ERROR,INVPRGTP                                                   
         B     TRAPERR                                                          
*                                                                               
VK160    MVC   SIRDPRG,1(R3)       DISPLAY PROGRAM TYPE NAME                    
*                                                                               
VK170    L     R5,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         LA    R5,2(R5)            FIRST PERIOD NAME                            
*                                                                               
         MVI   PERNUM,0            PERIOD                                       
         XC    SIRDPER,SIRDPER                                                  
         LA    R2,SIRPERH                                                       
         CLI   SIRPERH+5,0                                                      
         BNE   *+20                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK300                                                            
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         OC    SIRPER,=C'    '     PAD NAME WITH BLANKS                         
*                                                                               
VK180    CLC   SIRPER,1(R5)        LOOK FOR MATCH OF PERIOD NAME                
         BE    VK185                                                            
         LA    R5,5(R5)                                                         
         BCT   R1,VK180                                                         
         MVI   ERROR,INVBUYP                                                    
         B     TRAPERR                                                          
*                                                                               
VK185    MVC   PERNUM,0(R5)        SAVE THE PERIOD NUMBER                       
         MVC   SIRKMON,PERNUM                                                   
         OI    SIRKMON,SIRKBUYQ                                                 
*                                                                               
         LA    R4,KEY              BUILD THE PERIOD KEY                         
         MVC   KEY(13),SVKEY                                                    
         XC    SIRKMS,SIRKMS                                                    
         MVI   SIRKDPT,0                                                        
         MVI   SIRKMON,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR PERIOD KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,NOPERREC                                                   
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET THE PERIOD RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EPDCODEQ     LOOK FOR PERIOD ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R5                                                       
VK190    CLC   PERNUM,EPDNUM       LOOK FOR THE GIVEN PERIOD                    
         BE    VK195                                                            
         BAS   RE,NEXTEL                                                        
         BE    VK190                                                            
         MVI   ERROR,INVBUYP                                                    
         B     TRAPERR                                                          
*                                                                               
VK195    GOTO1 DATCON,DMCB,(3,EPDSTART),(5,SIRDPER)                             
         GOTO1 DATCON,DMCB,(3,EPDEND),(5,SIRDPER+9)                             
         MVI   SIRDPER+8,C'-'                                                   
*                                                                               
         MVC   SVUPFILE,EPDUPFIL   SAVE DEFAULT UPGRADE FOR PERIOD              
         MVC   SVUPGRD,EPDUPGRD                                                 
         MVC   SVUPFRBK,EPDUPFBK                                                
         MVC   SVUPINP,EPDUPINP                                                 
         DROP  R5                                                               
*                                                                               
         MVI   SIRKYEAR,0          YEAR IS ABSENT IN UPGRADE KEY                
         MVC   SIRKMON,PERNUM      BUT PERIOD IS THERE                          
         OI    SIRKMON,SIRKBUYQ                                                 
         MVC   SIRKMS,BMKTSTA      SO IS MKT/STA                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR STATION DEFAULT UPGRADE KEY         
         CLC   SIRKEY,KEYSAVE                                                   
         BE    VK200               WE HAVE IT                                   
*                                                                               
         MVC   SIRKEY,KEYSAVE      TRY MARKET DEFAULT UPGRADE                   
         XC    SIRKSTA,SIRKSTA                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   SIRKEY,KEYSAVE      TEST WE HAVE IT                              
         BNE   VK300               NO - USE PERIOD UPGRADE                      
*                                                                               
VK200    MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET STATION UPGRADE RECORD                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EUPCODEQ     STATION DEFAULT UPGRADE ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EUPELEM,R5                                                       
         MVC   SVUPFILE,EUPUPFIL   SAVE DEFAULT UPGRADE                         
         MVC   SVUPGRD,EUPUPGRD                                                 
         MVC   SVUPINP,EUPUPINP                                                 
*                                                                               
         CLI   SVUPGRD,3           TEST HUT OR PUT                              
         BNE   VK210               NO, NEITHER ONE                              
         CLI   SVUPGRD+1,C'P'      TEST PUT                                     
         BNE   VK210               NO, IT'S A HUT                               
*                                                                               
         ZIC   R0,CURYEAR          CURRENT YEAR                                 
         SR    R1,R1                                                            
         ICM   R1,8,SVUPGRD+2      RELATIVE PUT BOOK YEAR                       
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,SVUPGRD+2                                                     
*                                                                               
         MVC   SVUPINP(4),=C'PUT/' FORMAT PUT BOOK                              
         GOTO1 DATCON,DMCB,(3,SVUPGRD+2),(6,SVUPINP+4)                          
         MVC   SVUPINP+7(2),SVUPINP+8                                           
         MVC   SVUPINP+9(7),=C'       '                                         
*                                                                               
VK210    OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    VK300                                                            
         MVC   SVUPFRBK,EUPUPFBK                                                
         ZIC   R0,CURYEAR          CURRENT YEAR                                 
         SR    R1,R1                                                            
         ICM   R1,8,SVUPFRBK       RELATIVE SHARE BOOK YEAR                     
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,SVUPFRBK                                                      
         DROP  R5                                                               
*                                                                               
VK300    OI    SIRDPERH+6,FOUTTRN  XMIT PERIOD DATES                            
*                                                                               
VKX      XC    KEY,KEY             THE NSID KEY                                 
         MVC   KEY(13),SVKEY                                                    
*                                                                               
         OI    SIRSTAH+4,FINPVAL   MKT/STA HAS BEEN VALIDATED                   
         OI    SIRDPTH+4,FINPVAL   DPT HAS BEEN VALIDATED                       
         OI    SIRPERH+4,FINPVAL   PERIOD IS VALIDATED                          
         OI    SIRYRH+4,FINPVAL    YEAR HAS BEEN VALIDATED                      
         OI    SIRSCHH+4,FINPVAL   SCHEME HAS BEEN VALIDATED                    
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+8                 NO PROGRAM TYPE ON LIST SCREEN               
         OI    SIRPRGH+4,FINPVAL   PROGTYPE HAS BEEN VALIDATED                  
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       THE NSID KEY                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST IT'S THERE                              
         BE    *+16                                                             
         LA    R2,SIRSTAH          FIRST SELECT FIELD                           
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
         GOTO1 GETREC              NSID RECORD IN IO1                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NOINVFLG,C'Y'       TEST ANY INVENTORY IN LAST NSID              
         BNE   *+12                YES                                          
         CLI   ACTNUM,ACTSEL                                                    
         BE    SR                  NO INVENTORY - SELECT NEXT NSID              
*                                                                               
         CLI   FROMADD,C'Y'        TEST JUST CAME FROM ADD                      
         BE    DR25                YES - START FROM SCRATCH                     
         CLI   NEWKEY,C'Y'         TEST FIRST TIME THROUGH                      
         BE    DR25                YES                                          
         CLI   ACTNUM,ACTCHA       TEST ACTION CHANGE                           
         BE    CHANGE                                                           
         CLI   ACTNUM,ACTSEL       TEST ACTION SELECT                           
         BE    CHANGE                                                           
*                                                                               
         ZIC   R4,STARTSQN         INDEX TO 1ST SEQ. NO.                        
         LA    R4,SQNMS(R4)        POINT TO 1ST SEQ. NO. ON SCREEN              
*                                                                               
         LA    R3,DSKADD           TABLE OF DISK ADDRESSES                      
         LA    R2,SIRSELH          FIRST SELECT FIELD                           
*                                                                               
DR5      CLI   5(R2),0             TEST INPUT IN SELECT FIELD                   
         BE    DR15                NO                                           
         CLI   8(R2),C'*'          TEST WE'VE SELECTED THIS ALREADY             
         BE    DR15                YES - IGNORE                                 
         CLI   8(R2),C'S'          TEST DETAIL REQUESTED                        
         BE    DR12                                                             
         CLI   8(R2),C'C'          TEST DETAIL REQUESTED                        
         BE    DR12                                                             
*                                                                               
         CLI   SVUSECMP,C'Y'       TEST COMPETITION/ESTIMATE USER               
         BNE   DR10                NO - INVALID                                 
         CLI   8(R2),C'X'          TEST COMPETITION REQUESTED                   
         BE    DR12                                                             
         CLI   8(R2),C'E'          TEST ESTIMATING REQUESTED                    
         BE    DR12                                                             
*                                                                               
DR10     MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
DR12     CLI   0(R4),0             TEST RECORD WAS DELETED                      
         BNE   *+12                NO                                           
         MVI   ERROR,RECISDEL                                                   
         B     TRAPERR             YES - CANNOT SELECT IT                       
*                                                                               
         MVC   SVDETADR,0(R3)      SAVE DETAIL DISK ADDRESS                     
         MVC   AIO,AIO2            PUT DETAIL RECORD IN IO2                     
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),SYSFIL,(R3),AIO,DMWORK           
         CLI   8(R1),0             TEST RECORD FOUND                            
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         ST    R2,ASELFLDH         A(SELECT FIELD)                              
         CLI   8(R2),C'S'          TEST DETAIL REQUESTED                        
         BE    SELDET              YES - DISPLAY FOR SELECT                     
         CLI   8(R2),C'C'                                                       
         BE    SELDET                                                           
*                                                                               
         CLI   SVUSECMP,C'Y'       TEST COMPETITION/ESTIMATE USER               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   8(R2),C'X'          TEST COMPETITION REQUESTED                   
         BE    SELCOMP             YES - DISPLAY COMPETITION SCREEN             
         CLI   8(R2),C'E'          TEST ESTIMATING REQUESTED                    
         BE    SELESTIM            YES - DISPLAY ESTIMATE SCREEN                
         DC    H'0'                                                             
*                                                                               
DR15     LA    R4,1(R4)            A(NEXT SEQUENCE NO.)                         
         LA    R1,SQNMS                                                         
         AH    R1,LISTLEN          A(LAST SEQUENCE NO.)                         
         CR    R1,R4               TEST ANY MORE TO DISPLAY                     
         BE    DR25                NO - DISPLAY FROM BEGINNING                  
*                                                                               
         LA    R3,4(R3)            BUMP TO NEXT DISK ADDRESS                    
         OC    0(4,R3),0(R3)       TEST ANY MORE RECORDS ON SCREEN              
         BZ    DR35                NO                                           
         LA    RF,SIRPFH                                                        
         CR    R2,RF               TEST END OF SCREEN                           
         BE    DR35                YES                                          
*                                                                               
         LA    R1,6                NO. OF FIELDS PER ROW                        
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,*-6              BUMP TO NEXT SELECT FIELD                    
         B     DR5                                                              
*                                                                               
DR25     MVI   FROMADD,C'N'        OUT OF ADD MODE                              
         XC    SQNMS,SQNMS                                                      
         LA    R4,SQNMS                                                         
         MVI   SQNMDISP,0          FIRST SEQUENCE NO.                           
*                                                                               
         SR    R3,R3               COUNT DAY/TIME ELEMENTS                      
         L     R5,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    DR30                                                             
*                                                                               
         L     RF,=A(CLEAR)        NO INVENTORY - CLEAR SCREEN                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'Record contains no inventory'                     
         OI    CONHEADH+6,FOUTTRN  XMIT                                         
         OI    CONSERVH+6,FOUTTRN+FOUTMOD   SET MODIFIED AND TRANSMIT           
         OI    SIRSTAH+6,FOUTCUR   POSITION CURSOR                              
         MVI   RESTART,C'Y'                                                     
         MVI   NOINVFLG,C'Y'       NO INVENTORY FOUND                           
         B     DRX                                                              
*                                                                               
         USING EDPELEM,R5                                                       
DR30     CLI   PROGTYP,0           TEST PROGTYP GIVEN                           
         BE    *+14                                                             
         CLC   EDPPROG,PROGTYP     FILTER ON PROGTYP                            
         BNE   *+18                                                             
*                                                                               
         MVC   0(1,R4),EDPSEQ      FILL SEQUENCE NUMBER ARRAY                   
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)            INCREMENT ELEMENT COUNT                      
         BAS   RE,NEXTEL                                                        
         BE    DR30                                                             
         DROP  R5                                                               
*                                                                               
         STH   R3,LISTLEN          TOTAL NUMBER OF DAY/TIMES IN LIST            
         OC    LISTLEN,LISTLEN     TEST ANY ELEMENTS LEFT AFTER FILTER          
         BNZ   DR35                                                             
*                                                                               
         L     RF,=A(CLEAR)        NO INVENTORY - CLEAR SCREEN                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'No inventory of given program type'               
         OI    CONHEADH+6,FOUTTRN  XMIT                                         
         OI    CONSERVH+6,FOUTTRN+FOUTMOD   SET MODIFIED AND TRANSMIT           
         OI    SIRPRGH+6,FOUTCUR   POSITION CURSOR                              
         MVI   RESTART,C'Y'                                                     
         MVI   NOINVFLG,C'Y'       NO INVENTORY FOUND                           
         B     DRX                                                              
*                                                                               
DR35     L     RF,=A(CLEAR)        CLEAR SCREEN                                 
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         LA    R2,SIRSELH                                                       
         MVC   AIO,AIO2            USE AIO2 FOR DETAIL RECORDS                  
         XC    DSKADD(64),DSKADD   CLEAR DISK ADDRESS TABLE                     
         MVI   DSKADDX,0           START AT BEGINNING OF D/A TABLE              
         MVC   STARTSQN,SQNMDISP   INDEX TO 1ST SEQUENCE NO.                    
*                                                                               
DR40     XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       BUILD THE DETAIL KEY                         
         LA    R5,KEY                                                           
         USING SIRKEY,R5                                                        
         ZIC   R4,SQNMDISP                                                      
         LA    R4,SQNMS(R4)                                                     
         CLI   0(R4),0             MAKE SURE THERE'S A SEQUENCE NO.             
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SIRKSEQ,0(R4)       SEQUENCE NUMBER                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         DROP  R5                                                               
*                                                                               
         CLC   KEY(13),KEYSAVE     TEST RECORD FOUND                            
         BNE   NODETAIL                                                         
*        BE    *+6                                                              
*        DC    H'0'                IT'S GOT TO BE THERE                         
*                                                                               
         GOTO1 GETREC              DETAIL RECORD                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDPELEM,R5                                                       
*                                                                               
         ZIC   R1,DSKADDX          DISP INTO D/A TABLE                          
         LA    R1,DSKADD(R1)                                                    
         MVC   0(4,R1),KEY+14      SAVE THE DISK ADDRESS                        
*                                                                               
         ZIC   R0,0(R2)            DAY FIELD                                    
         AR    R2,R0                                                            
         XC    WORK,WORK           DAY                                          
         GOTO1 UNDAY,DMCB,EDPDAY,WORK                                           
         MVC   8(7,R2),WORK                                                     
         OI    6(R2),FOUTTRN       XMIT                                         
         OI    4(R2),FINPVAL       VALIDATED                                    
*                                                                               
         ZIC   R0,0(R2)            TIME                                         
         AR    R2,R0                                                            
         XC    WORK,WORK                                                        
         GOTO1 UNTIME,DMCB,EDPTIME,WORK                                         
         MVC   8(11,R2),WORK                                                    
         OI    6(R2),FOUTTRN       XMIT                                         
         OI    4(R2),FINPVAL       VALIDATED                                    
*                                                                               
         ZIC   R0,0(R2)            PROGTYP                                      
         AR    R2,R0                                                            
         MVC   8(1,R2),EDPPROG                                                  
         OI    6(R2),FOUTTRN       XMIT                                         
         OI    4(R2),FINPVAL       VALIDATED                                    
         DROP  R5                                                               
*                                                                               
         ZIC   R0,0(R2)            COST (30'S)                                  
         AR    R2,R0                                                            
         OI    4(R2),FINPVAL       VALIDATED                                    
         L     R5,AIO                                                           
         MVI   ELCODE,EECCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR50                                                             
*                                                                               
         USING EECELEM,R5                                                       
         SR    R0,R0                                                            
         ICM   R1,15,EECCOST1      FIRST COST                                   
         D     R0,=F'100'                                                       
         EDIT  (R1),(5,WORK),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   8(5,R2),WORK                                                     
         OI    6(R2),FOUTTRN       XMIT                                         
         DROP  R5                                                               
*                                                                               
DR50     ZIC   R0,0(R2)            UPG/DEMO/PROG FIELD                          
         AR    R2,R0                                                            
         OI    4(R2),FINPVAL       VALIDATED                                    
         L     R5,AIO                                                           
         MVI   ELCODE,EUPCODEQ     UPGRADE ELEMEMT                              
         BAS   RE,GETEL                                                         
         BNE   DR90                                                             
*                                                                               
         USING EUPELEM,R5                                                       
         MVI   WORK,C' '           BUILD UPGRADE EXPRESSION                     
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EUPUPFIL                                               
         MVC   WORK+4(16),EUPUPINP                                              
*                                                                               
         OC    EUPUPINP,EUPUPINP   CANADIAN UPGRADE EXPRESSION?                 
         BNZ   *+18                                                             
         LA    R3,WORK+4           YES                                          
         MVC   0(3,R3),=C'BK/'                                                  
         B     DR55                                                             
*                                                                               
         LA    R3,WORK+21                                                       
         OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    DR60                NO                                           
         BAS   RE,SETCOMMA                                                      
         MVC   0(3,R3),=C'BK='                                                  
*                                                                               
DR55     GOTO1 DATCON,DMCB,(3,EUPUPFBK),(6,3(R3))                               
         LA    R3,9(R3)                                                         
         CLI   EUPLEN,EUPLENXQ     NEW VERSION OF ELEMENT?                      
         BL    DR60                NO                                           
*                                                                               
         LA    R0,3                MAXIMUM OF 3 MORE BOOKS                      
         LA    R4,EUPUPMBK         EXTRA BOOKS FOR CANADA                       
DR57     OC    0(2,R4),0(R4)       ANY MORE?                                    
         BZ    DR60                                                             
         MVI   0(R3),C'/'          BOOK DELIMITER                               
         GOTO1 DATCON,DMCB,(3,0(R4)),(6,1(R3))                                  
         LA    R3,7(R3)                                                         
         LA    R4,2(R4)                                                         
         BCT   R0,DR57                                                          
         DROP  R5                                                               
*                                                                               
DR60     L     R5,AIO                                                           
         MVI   ELCODE,EOVCODEQ     OVERRIDE ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DR80                                                             
*                                                                               
         USING EOVELEM,R5                                                       
         OC    EOVUPDAY,EOVUPDAY   TEST DAY/TIME OVERRIDE                       
         BZ    DR70                NO                                           
*                                                                               
         BAS   RE,SETCOMMA                                                      
         MVC   0(3,R3),=C'DT='                                                  
         GOTO1 UNDAY,DMCB,EOVUPDAY,3(R3)                                        
         LA    R3,11(R3)                                                        
*                                                                               
         BAS   RE,SETCOMMA                                                      
         BCTR  R3,0                                                             
         MVI   0(R3),C'/'                                                       
         GOTO1 UNTIME,DMCB,EOVUPTIM,1(R3)                                       
         LA    R3,14(R3)                                                        
*                                                                               
DR70     OC    EOVUPSTA,EOVUPSTA   TEST STATION OVERRIDE                        
         BZ    DR80                NO                                           
*                                                                               
         BAS   RE,SETCOMMA                                                      
         MVC   0(3,R3),=C'ST='                                                  
         MVC   3(4,R3),EOVUPSTA                                                 
*                                                                               
         CLI   EOVLEN,EOVLENXQ     NEW VERSION OF ELEMENT?                      
         BL    DR80                NO                                           
         OC    EOVUPSTX,EOVUPSTX                                                
         BZ    DR80                                                             
         MVC   7(3,R3),EOVUPSTX                                                 
         DROP  R5                                                               
*                                                                               
DR80     CLI   L'SIRUPG+WORK,C' '  TEST IF UPGRADE FITS                         
         BE    *+14                                                             
         MVC   8(20,R2),=C'* UPGRADE TOO LONG *'                                
         B     DR120                                                            
*                                                                               
         MVC   8(L'SIRUPG,R2),WORK EBCDIC UPGRADE EXPRESSION                    
         B     DR120                                                            
*                                                                               
DR90     CLI   SVUSECMP,C'Y'       TEST COMPETITION USER                        
         BE    DR120               YES - ONLY UPGRADES ARE ALLOWED HERE         
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EDOCODEQ     DEMO OVERRIDE ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   DR115                                                            
*                                                                               
         L     R4,AIO3                                                          
         LA    R4,1000(R4)                                                      
         XC    0(200,R4),0(R4)                                                  
*                                                                               
DR95     L     RF,=A(CNVRT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC)      CONVERT BINARY CODES TO NAMES                
         BAS   RE,NEXTEL                                                        
         BE    DR95                                                             
*                                                                               
         L     R1,AIO3                                                          
         LA    R1,1000(R1)                                                      
         OC    0(10,R1),0(R1)      ANY DEMO OVERRIDES                           
         BZ    DR120                                                            
*                                                                               
         LA    R3,60(R1)           R1 - START OF BLOCK AREA                     
         CLI   0(R3),C','          R3 - END OF BLOCK AREA                       
         BNE   DR100                                                            
         MVI   0(R3),C' '                                                       
         B     DR110                                                            
*                                                                               
DR100    BCTR  R3,0                                                             
         CR    R1,R3               R3 MUST NOT BE LESS THEN R1                  
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R3),C','                                                       
         BNE   DR100                                                            
         MVI   0(R3),C' '                                                       
*                                                                               
DR110    SR    R3,R1               LENGTH OF OVERRIDE                           
         ZIC   R0,0(R2)            LENGTH OF FIELD                              
         SH    R0,=H'16'           MINUS HEADER AND EXTENSION                   
         CR    R3,R0               TEST IF OVERRIDE FITS                        
         BNH   *+14                                                             
         MVC   8(21,R2),=C'* OVERRIDE TOO LONG *'                               
         B     DR120                                                            
         MVC   8(L'SIRUPG,R2),0(R1) MOVE IN OVERRIDES                           
         B     DR120                                                            
*                                                                               
DR115    L     R5,AIO                                                           
         MVI   ELCODE,EPRCODEQ     PROGRAMMING ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DR120                                                            
*                                                                               
         USING EPRELEM,R5          DISPLAY PROGRAMMING                          
         MVC   8(5,R2),=C'PROG='                                                
         MVC   13(17,R2),EPRTEXT                                                
         DROP  R5                                                               
*                                                                               
DR120    OI    6(R2),FOUTTRN       XMIT UPG/DEMO/PROG FIELD                     
         ZIC   R0,0(R2)            NEXT SELECT FIELD                            
         AR    R2,R0                                                            
         ZIC   R1,DSKADDX                                                       
         LA    R1,4(R1)            INCREMENT D/A POINTER                        
         STC   R1,DSKADDX                                                       
*                                                                               
         MVI   NEWKEY,C'N'                                                      
         OI    CONSERVH+6,FOUTTRN+FOUTMOD   SET MODIFIED AND TRANSMIT           
         XC    CONHEAD,CONHEAD     A MESSAGE WILL BE PLACED HERE                
         OI    CONHEADH+6,FOUTTRN  XMIT                                         
         OI    SIRSELH+6,FOUTCUR   POSITION CURSOR                              
*                                                                               
         ZIC   R4,SQNMDISP                                                      
         LA    R4,1(R4)            INCREMENT SEQUENCE NO. COUNTER               
         STC   R4,SQNMDISP                                                      
         LA    R1,SQNMS(R4)                                                     
         CLI   0(R1),0             TEST ANY MORE RECORDS TO DISPLAY             
         BNE   DR130               YES                                          
*                                                                               
         MVI   RESTART,C'Y'        START DISPLAY FROM BEGINNING AGAIN           
         CLI   ACTNUM,ACTDIS       TEST ACTION DISPLAY                          
         BNE   DR140                                                            
         MVI   RESTART,C'N'                                                     
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(43),=C'End of list - select or hit enter for fir+        
               st'                                                              
         B     DRX                                                              
*                                                                               
DR130    MVI   RESTART,C'N'                                                     
         LA    RF,SIRPFH                                                        
         CR    R2,RF               TEST END OF SCREEN                           
         BNE   DR40                NO                                           
*                                                                               
         CLI   ACTNUM,ACTDIS       SCREEN IS FULL - IS IT DISP                  
         BNE   DR140                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'Hit enter to continue list'                       
*                                                                               
DR140    CLI   ACTNUM,ACTCHA       TEST ACTION CHANGE                           
         BNE   *+20                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(17),=C'Now enter changes'                                
         B     DR150                                                            
*                                                                               
         CLI   ACTNUM,ACTSEL       TEST ACTION SELECT                           
         BNE   DR170                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'Selection displayed - now enter changes'          
*                                                                               
DR150    LA    RF,SIRPFH                                                        
         CR    R2,RF               TEST END OF SCREEN                           
         BE    DR160               YES                                          
         OI    1(R2),FATBPROT      PROTECT REMAINING FIELDS                     
         NI    1(R2),X'FF'-FOUTMOD UNMODIFY                                     
         OI    6(R2),FOUTTRN       XMIT                                         
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         B     DR150                                                            
*                                                                               
DR160    CLI   MODSCRN,C'Y'        TEST ANY CHANGES ON LAST SCREEN              
         BNE   DRX                                                              
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'Changes processed - enter more changes'           
         MVI   MODSCRN,C'N'        RESET THE FLAG                               
         B     DRX                                                              
*                                                                               
DR170    CLI   ACTNUM,ACTCOPY      TEST ACTION COPY                             
         BNE   DRX                                                              
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(18),=C'NSID record copied'                               
*                                                                               
DRX      MVC   AIO,AIO1            RESTORE AIO                                  
         CLI   GCMODE,C'S'         TEST GENCON IS SLAVED                        
         BNE   DRX1                                                             
         GOTO1 SAVEUWKA            YES - MUST SAVE SYSD                         
*                                                                               
DRX1     B     XIT                                                              
         SPACE 5                                                                
SETCOMMA CLI   0(R3),C' '          BACK UP TO NON-BLANK/NULL                    
         BH    *+8                                                              
         BCT   R3,SETCOMMA                                                      
         MVI   1(R3),C','          EDIT IN COMMA                                
         LA    R3,2(R3)                                                         
         BR    RE                  R3=NEXT AVAILABLE POSITION                   
         EJECT                                                                  
* ADD NEW DAY/TIMES                                                             
*                                                                               
AD       CLI   FROMADD,C'Y'        TEST WE'VE BEEN HERE BEFORE                  
         BE    AD15                YES                                          
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         OI    CONHEADH+6,FOUTTRN                                               
         OI    SIRDAYH+6,FOUTCUR   POSITION CURSOR                              
         LA    R2,SIRSELH          PROTECT ALL SELECT FIELDS                    
*                                                                               
AD5      OI    1(R2),FATBPROT      PROTECT                                      
         NI    1(R2),X'FF'-FOUTMOD UNMODIFY                                     
         MVI   5(R2),0             CLEAR                                        
         MVI   8(R2),0                                                          
         OI    6(R2),FOUTTRN       XMIT                                         
*                                                                               
         LA    R1,5                NO. OF DATA FIELDS PER ROW                   
         SR    R0,R0                                                            
AD10     IC    R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         NI    1(R2),X'FF'-FATBPROT UNPROTECT                                   
         OI    6(R2),FOUTTRN       XMIT                                         
         BCT   R1,AD10                                                          
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT SELECT                          
         AR    R2,R0                                                            
         LA    RF,SIRPFH                                                        
         CR    R2,RF               TEST END OF SCREEN                           
         BNE   AD5                 NO - PROTECT NEXT SELECT FIELD               
*                                                                               
AD15     MVI   FROMADD,C'Y'        WE'RE DOING AN ADD NOW                       
         MVI   DAY,0               NOTHING TO DITTO DOWN YET                    
         LA    R2,SIRSELH          FIRST FIELD                                  
         L     R5,AIO              NSID RECORD IN AIO1                          
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       NSID KEY                                     
         MVI   RDUPDATE,C'Y'  <-- WAS C'N', CAUSE OF SEQUENCING PROB?           
         GOTO1 HIGH                LOOK FOR NSID KEY                            
         CLC   KEY(13),KEYSAVE     TEST IT IS THERE                             
         BE    AD20                YES                                          
*                                                                               
         XC    0(256,R5),0(R5)     BUILD NEW NSID RECORD                        
         USING SIRREC,R5                                                        
         MVC   SIRKEY,KEYSAVE      NSID KEY                                     
         MVC   SIRRLEN,=H'24'      CURRENT RECORD LENGTH                        
         MVI   ADDNSID,C'Y'        WE'LL DO AN ADDREC LATER                     
         B     AD25                                                             
         DROP  R5                                                               
*                                                                               
AD20     MVI   ADDNSID,C'N'        WE'LL DO A PUTREC LATER                      
         GOTO1 GETREC              NSID RECORD IN IO1                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
AD25     ST    R2,ASELFLDH         A(SELECT FIELD)                              
         ZIC   R0,0(R2)            BUMP TO DAY FIELD                            
         AR    R2,R0                                                            
         CLI   5(R2),0             ANY DAY GIVEN?                               
         BNE   AD30                YES                                          
*                                                                               
         IC    R0,0(R2)            BUMP TO TIME FIELD                           
         AR    R2,R0                                                            
         CLI   5(R2),0             ANY TIME GIVEN?                              
         BNE   AD28                YES -- DON'T IGNORE THIS ROW                 
*                                                                               
         IC    R0,0(R2)            BUMP TO PRGTYPE                              
         AR    R2,R0                                                            
         IC    R0,0(R2)            BUMP TO COST                                 
         AR    R2,R0                                                            
         IC    R0,0(R2)            BUMP TO UPGRADE                              
         AR    R2,R0                                                            
         B     AD230               IGNORE THIS ROW - DO NEXT ONE                
*                                                                               
AD28     CLI   DAY,0               ANY DAY FROM DITTOING DOWN?                  
         BNE   AD35                YES                                          
         L     R2,ASELFLDH         NO -- MUST HAVE DAY                          
         IC    R0,0(R2)            BUMP TO DAY FIELD                            
         AR    R2,R0                                                            
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
AD30     ZIC   R3,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R3),8(R2)),DAY,WORK                                
         CLI   DAY,0                                                            
         BNE   AD35                                                             
         MVI   ERROR,INVDAY                                                     
         B     TRAPERR                                                          
*                                                                               
AD35     L     R2,ASELFLDH         A(SELECT FIELD)                              
         CLI   8(R2),C'*'          TEST ROW WAS ALREADY PROCESSED               
         BNE   AD40                NO                                           
*                                                                               
         ZIC   R0,0(R2)            BUMP TO DAY FIELD                            
         AR    R2,R0                                                            
         IC    R0,0(R2)            BUMP TO TIME                                 
         AR    R2,R0                                                            
         IC    R0,0(R2)            BUMP TO PRGTYPE                              
         AR    R2,R0                                                            
         IC    R0,0(R2)            BUMP TO COST                                 
         AR    R2,R0                                                            
         IC    R0,0(R2)            BUMP TO UPGRADE                              
         AR    R2,R0                                                            
         B     AD230               IGNORE THIS ROW - DO NEXT ONE                
*                                                                               
AD40     ZIC   R0,0(R2)            BUMP TO DAY FIELD                            
         AR    R2,R0                                                            
         IC    R0,0(R2)            BUMP TO TIME FIELD                           
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R3),8(R2)),TIME                                    
         MVI   ERROR,INVTIME                                                    
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
         CLC   =C'NONE',TIME                                                    
         BE    TRAPERR                                                          
         CLC   =C'VARY',TIME                                                    
         BE    TRAPERR                                                          
*                                                                               
         L     R5,AIO              NSID RECORD                                  
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         MVI   SEQNUM,1            FIRST DAY/TIME ELEMENT                       
         B     AD70                                                             
*                                                                               
         XC    SQNMS,SQNMS         CLEAR USED SEQUENCE NUMBER ARRAY             
         USING EDPELEM,R5                                                       
*                                                                               
AD50     CLC   DAYTIME,EDPDAY      LOOK FOR THE GIVEN DAY/TIME                  
         BE    DUPSEQ              ALREADY EXISTS - ERROR                       
*                                                                               
         ZIC   R0,EDPSEQ                                                        
         LA    R3,SQNMS                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,*-4                                                           
         MVI   0(R3),X'FF'         MARK SEQUENCE NUMBER AS USED                 
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    AD50                                                             
         DROP  R5                                                               
*                                                                               
         LA    R1,1                                                             
         LA    R3,SQNMS+1          LOOK FOR FIRST AVAILABLE SEQNUM              
AD60     CLI   0(R3),0                                                          
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         B     AD60                                                             
         STC   R1,SEQNUM           HERE IT IS                                   
*                                                                               
AD70     XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING SIRREC,R5                                                        
         L     R4,AIO              NSID RECORD                                  
         MVC   SIRKEY,0(R4)                                                     
         CLI   SEQNUM,0            MAKE SURE THERE'S A SEQUENCE NO.             
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SIRKSEQ,SEQNUM      DETAIL KEY                                   
         MVC   AIO,AIO2            BUILD DETAIL RECORD IN IO2                   
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS AS WELL            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   AD80                                                             
*                                                                               
         CLI   DMCB+8,X'02'        TEST KEY WAS DELETED                         
         BE    AD75                MUST HAVE BEEN                               
         MVC   CONHEAD(40),=C'** RECORD SEQ ERROR, CONTACT DDS ASAP **'         
         NI    CONHEADH+6,X'FF'-X'04'  HIGHLIGHT THE ERROR                      
         OI    CONHEADH+6,X'80'+X'08'                                           
         DC    H'0',C'$ABEND'                                                   
*                                                                               
AD75     MVC   SAVEKEY,KEY         HANG ON TO DELETED KEY                       
         GOTO1 GETREC              GET THE DELETED RECORD                       
         CLI   DMCB+8,X'02'        RECORD MUST HAVE BEEN DELETED                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO                                                           
         NI    SIRRCNTL,X'7F'      UNDELETE THE RECORD                          
*                                                                               
         MVI   ELCODE,0            LOOK FOR ANY ELEMENT AT ALL                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   0(R5),X'FF'         MARK ALL ELEMENTS FOR DELETION               
         BAS   RE,NEXTEL                                                        
         CLI   0(R5),X'F1'         EXCEPT THE ACTIVITY ELEMENT                  
         BNE   *-12                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             REMOVE ALL ELEMENTS                          
         MVI   ADDDET,C'N'         WE'LL DO PUTREC LATER                        
         B     AD90                                                             
*                                                                               
AD80     L     R5,AIO              BUILD NEW RECORD                             
         XC    0(256,R5),0(R5)                                                  
         MVC   SIRKEY,KEYSAVE      THE NEW DETAIL KEY                           
         MVC   SIRRLEN,=H'24'      CURRENT RECORD LENGTH                        
         MVI   ADDDET,C'Y'         WE'LL DO ADDREC LATER                        
         DROP  R5                                                               
*                                                                               
AD90     NI    DMINBTS,X'F7'       DON'T READ FOR DELETED                       
         ZIC   R0,0(R2)            BUMP TO PROGRAM TYPE                         
         AR    R2,R0                                                            
*                                                                               
         MVI   PROGTYPE,0          ASSUME NO PROGRAM TYPE                       
         CLI   5(R2),0             TEST PROGTYPE GIVEN                          
         BE    AD120               NO                                           
*                                                                               
         LA    R3,PRGTYPSV         LOOK FOR MATCH IN LIST                       
AD100    CLC   0(1,R3),8(R2)                                                    
         BE    AD110                                                            
         LA    R3,8(R3)                                                         
         CLI   0(R3),0             TEST END OF TABLE                            
         BNE   AD100               NO                                           
         MVI   ERROR,INVPRGTP                                                   
         B     TRAPERR                                                          
*                                                                               
AD110    MVC   PROGTYPE,8(R2)      PUT PROGTYP CODE IN ELEMENT                  
*                                                                               
AD120    LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EDPELEM,R5                                                       
         MVI   EDPCODE,EDPCODEQ    DAY/TIME/PROGTYP ELEMENT                     
         MVI   EDPLEN,EDPLENEQ     ELEMENT LENGTH                               
         MVC   EDPSEQ,SEQNUM       SEQUENCE NUMBER                              
         MVC   EDPDAY(5),DAYTIME   SAVE THE DAY/TIME                            
         MVC   EDPPROG,PROGTYPE    SAVE THE NEW PROGRAM TYPE                    
         GOTO1 ADDELEM                                                          
*                                                                               
         MVC   AIO,AIO1            NOW ADD ELEMENT TO NSID RECORD               
         MVI   EDPINDEX,1          ASSUME IT'S THE FIRST ONE                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
         MVC   AIO,AIO2            BACK TO DETAIL RECORD                        
*                                                                               
         ZIC   R0,0(R2)            BUMP TO COST                                 
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST A COST GIVEN                            
         BE    AD130               NO                                           
*                                                                               
         ZIC   R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(C'N',8(R2)),(R4)                                   
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         L     R1,4(R1)                                                         
         MH    R1,=H'100'                                                       
         STCM  R1,15,COST                                                       
*                                                                               
         USING EECELEM,R5                                                       
         LA    R5,ELEM             CREATE 30'S COST ELEMENT                     
         XC    ELEM,ELEM                                                        
         MVI   EECCODE,EECCODEQ                                                 
         MVI   EECLEN,EECLENEQ                                                  
         MVI   EECINDEX,1                                                       
         MVI   EECSLN,30                                                        
         MVC   EECCOST1,COST                                                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
AD130    ZIC   R0,0(R2)            BUMP TO UPG/DEMO/PROG FIELD                  
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST ANY DATA                                
         BE    AD170               NO                                           
*                                                                               
         CLI   SVUSECMP,C'Y'       TEST COMPETITION/ESTIMATE USER               
         BE    AD150               YES - ONLY UPGRADES ALLOWED HERE             
         CLC   =C'UPT=',8(R2)      TEST AN UPGRADE IS BEING GIVEN               
         BE    AD150               YES                                          
         CLC   =C'PROG=',8(R2)     TEST PROGRAMMING IS GIVEN                    
         BE    AD165               YES                                          
*                                                                               
         LA    RE,BLOCK            SET UP FOR DEMOVAL                           
         LA    RF,480                                                           
         XCEF                                                                   
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
         SR    R3,R3               FIELD NUMBER (FOR ERRORS)                    
         LA    R4,BLOCK+256        R4 - SCAN OUT AREA                           
         GOTO1 SCANNER,DMCB,(R2),(R4)                                           
*                                                                               
AD140    LA    R3,1(R3)            INCREMENT FIELD ERROR NUMBER                 
         XC    DEMWRK(40),DEMWRK                                                
         ZIC   R1,0(R4)            L'DEMO NAME                                  
         STC   R1,DEMWRK+5                                                      
         LA    R1,8(R1)            L'DEMONAME + HEADER                          
         STC   R1,DEMWRK                                                        
         MVC   DEMWRK+8(10),12(R4) DEMNAME TO DEMWRK                            
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         GOTO1 CDEMOVAL,DMCB,(1,DEMWRK),DEMWRK+20,(C'S',BLOCK)                  
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               TEST DEMO ERROR                              
         BZ    DEMOERR                                                          
         DROP  R5                                                               
*                                                                               
         CLI   1(R4),0             L'2ND SCAN OUTFIELD (DEMVALUE)               
         BE    RTNGERR                                                          
         ZIC   R5,1(R4)                                                         
         GOTO1 CASHVAL,DMCB,(1,22(R4)),(R5)                                     
         CLI   0(R1),X'FF'                                                      
         BE    RTNGERR                                                          
         CLC   4(4,R1),=F'32678'   MAX CASHVALUE IN HALF WRD                    
         BH    RTNGERR                                                          
*                                                                               
         LA    R5,ELEM                                                          
         USING EDOELEM,R5                                                       
         XC    ELEM,ELEM                                                        
         MVI   EDOCODE,EDOCODEQ                                                 
         MVI   EDOLEN,EDOLENEQ                                                  
         MVC   EDODEMO,DEMWRK+21   LAST 2 BYTES OF 3 BYTE CODE                  
         MVC   EDOVALUE,6(R1)      AFTER CASHVAL,2ND PARM=VALUE                 
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R4,32(R4)           INCREMENT POSITION IN SCANNER BLOCK          
         OC    0(7,R4),0(R4)                                                    
         BNZ   AD140                                                            
         B     AD170                                                            
         DROP  R5                                                               
*                                                                               
AD150    GOTO1 VALIUPG,DMCB,BLOCK+256                                           
         OC    CONHEAD,CONHEAD     TEST FOR ERROR MESSAGE                       
         BNZ   SOLONG              YES - LEAVE                                  
         LA    R3,BLOCK+256                                                     
         USING SPDEMUPD,R3                                                      
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EUPELEM,R5                                                       
         MVI   EUPCODE,EUPCODEQ                                                 
         MVI   EUPLEN,EUPLENXQ                                                  
         MVC   EUPUPFIL,SPUPFIL                                                 
         MVC   EUPUPGRD,SPUPTYPE                                                
         MVC   EUPUPFBK,SPUPFBK                                                 
         MVC   EUPUPINP,SPUPPRG                                                 
         MVC   EUPUPMBK(6),SPUPFBKL                                             
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         OC    SPUPUDAY,SPUPUDAY                                                
         BNZ   AD160                                                            
         OC    SPUPUTIM,SPUPUTIM                                                
         BNZ   AD160                                                            
         OC    SPUPSTA,SPUPSTA                                                  
         BZ    AD170                                                            
*                                                                               
AD160    LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EOVELEM,R5                                                       
         MVI   EOVCODE,EOVCODEQ                                                 
         MVI   EOVLEN,EOVLENXQ                                                  
         MVC   EOVUPDAY,SPUPUDAY                                                
         MVC   EOVUPTIM,SPUPUTIM                                                
         MVC   EOVUPSTA,SPUPSTA                                                 
         GOTO1 ADDELEM                                                          
         B     AD170                                                            
         DROP  R3,R5                                                            
*                                                                               
AD165    CLI   5(R2),22            'PROG=' PLUS 17 CHARACTER MAX LEN            
         BH    PROGERR                                                          
         CLI   5(R2),6                                                          
         BL    PROGERR                                                          
*                                                                               
         LA    R5,ELEM             BUILD PROGRAMMING ELEMENT                    
         XC    ELEM,ELEM                                                        
         USING EPRELEM,R5                                                       
         MVI   EPRCODE,EPRCODEQ                                                 
         MVI   EPRLEN,EPRLENEQ                                                  
         MVC   EPRTEXT,13(R2)                                                   
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
AD170    CLI   ADDDET,C'Y'         TEST ADD NEW DETAIL RECORD                   
         BNE   AD180                                                            
         GOTO1 ADDREC              ADD THE NEW DETAIL RECORD                    
         CLI   DMCB+8,0                                                         
         BE    AD190                                                            
         DC    H'0'                                                             
*                                                                               
AD180    GOTO1 PUTREC              UNDELETE AND UPDATE NEW RECORD               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(18),SAVEKEY     THE DELETED KEY                              
         NI    KEY+13,X'7F'        UNDELETE THE KEY                             
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
AD190    MVI   MODSCRN,C'Y'        SOMETHING HAPPENED                           
         CLI   ADDNSID,C'Y'        TEST ADD NEW NSID RECORD                     
         BNE   AD200                                                            
         MVC   AIO,AIO1                                                         
         L     R5,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R5)                                                    
         GOTO1 ADDREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADDNSID,C'N'        WON'T NEED TO ADDREC ANY MORE                
         B     AD230                                                            
*                                                                               
AD200    XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       THE NSID KEY                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3            READ INTO USELESS AREA                       
         GOTO1 GETREC              SET DMWORK FOR NSID RECORD                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1            THE UNSORTED NSID RECORD                     
         L     R5,AIO                                                           
         USING EDPELEM,R5                                                       
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENTS                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R3,R3               COUNT THE ELEMENTS                           
*                                                                               
AD210    XI    EDPDAY,X'FF'        MASSAGE DAY/TIME FOR SORTING                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EDPSTRT                                                     
         CH    R0,=H'600'                                                       
         BNL   *+12                                                             
         AH    R0,=H'2400'                                                      
         STCM  R0,3,EDPSTRT                                                     
*                                                                               
         OC    EDPENDT,EDPENDT                                                  
         BZ    AD215                                                            
         CLC   EDPENDT,=C'CC'                                                   
         BE    AD215                                                            
         ICM   R0,3,EDPENDT                                                     
         CH    R0,=H'600'                                                       
         BNL   AD215                                                            
         AH    R0,=H'2400'                                                      
         STCM  R0,3,EDPENDT                                                     
*                                                                               
AD215    LA    R3,1(R3)            INCREMENT ELEMENT COUNT                      
         BAS   RE,NEXTEL                                                        
         BE    AD210                                                            
*                                                                               
         L     R5,AIO                                                           
         BAS   RE,GETEL            R5 - A(FIRST DAY/TIME)                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 XSORT,DMCB,(R5),(R3),10,5,4                                      
         SR    R3,R3                                                            
*                                                                               
AD220    LA    R3,1(R3)                                                         
         STC   R3,EDPINDEX         SORTING SEQUENCE NO.                         
         XI    EDPDAY,X'FF'        RESTORE DAY CODE                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EDPSTRT        RESTORE START TIME                           
         CH    R0,=H'2400'                                                      
         BNH   *+12                                                             
         SH    R0,=H'2400'                                                      
         STCM  R0,3,EDPSTRT                                                     
*                                                                               
         CLC   EDPENDT,=C'CC'                                                   
         BE    AD225                                                            
         ICM   R0,3,EDPENDT        RESTORE END TIME                             
         CH    R0,=H'2400'                                                      
         BNH   AD225                                                            
         SH    R0,=H'2400'                                                      
         STCM  R0,3,EDPENDT                                                     
*                                                                               
AD225    BAS   RE,NEXTEL                                                        
         BE    AD220                                                            
         GOTO1 PUTREC              UPDATE THE NSID RECORD                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
AD230    L     RF,ASELFLDH         A(SELECT FIELD)                              
         MVI   8(RF),C'*'          MARK LINE AS PROCESSED                       
         OI    6(RF),FOUTTRN       XMIT                                         
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT SELECT                          
         AR    R2,R0                                                            
         LA    R0,SIRPFH                                                        
         CR    R2,R0               TEST END OF SCREEN                           
         BNE   AD25                NO - PROCESS NEXT RECORD                     
*                                                                               
         LA    R2,SIRSELH                                                       
AD240    MVI   8(R2),0             CLEAR ASTERISKS FROM SELECT FIELD            
         OI    6(R2),FOUTTRN       XMIT                                         
*                                                                               
         LA    R1,6                NO. OF FIELDS PER ROW                        
         SR    R0,R0                                                            
         IC    R0,0(R2)            BUMP TO NEXT SELECT                          
         AR    R2,R0                                                            
         BCT   R1,*-6                                                           
*                                                                               
         LA    RF,SIRPFH                                                        
         CR    R2,RF               TEST END OF SCREEN                           
         BNE   AD240               NO - PROTECT NEXT SELECT FIELD               
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   CONHEAD(29),=C'Please enter data as required'                    
         CLI   MODSCRN,C'Y'        TEST ANYTHING HAPPENED                       
         BNE   ADX                                                              
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'New records have been added to file'              
         OI    CONHEADH+6,FOUTTRN                                               
         OI    SIRDAYH+6,FOUTCUR   POSITION CURSOR                              
         MVI   MODSCRN,C'N'                                                     
*                                                                               
ADX      B     XIT                                                              
         EJECT                                                                  
* CHANGE RECORDS                                                                
*                                                                               
CHANGE   MVC   SQNMDISP,STARTSQN   INDEX TO 1ST DAY/TIME ON SCREEN              
         LA    R2,SIRSELH          FIRST SELECT FIELD                           
         MVI   DSKADDX,0                                                        
         LA    R3,DSKADD           BEGINNING OF D/A LIST                        
*                                                                               
CH5      CLI   5(R2),0             TEST ANY SELECT FIELD INPUT                  
         BE    CH15                NO                                           
*                                                                               
         CLI   8(R2),C'*'          TEST WE'VE SELECTED HERE EARLIER             
         BE    CH15                YES - TREAT AS POSSIBLE CHANGE               
         CLI   8(R2),C'S'          TEST DETAIL SELECT                           
         BE    CH15                YES - VALIDATE, THEN SELECT                  
         CLI   8(R2),C'C'          TEST DETAIL SELECT                           
         BE    CH15                YES - VALIDATE, THEN SELECT                  
*                                                                               
         CLI   8(R2),C'D'          TEST DELETE                                  
         BNE   *+12                                                             
         BAS   RE,DELDET           DELETE THE RECORD                            
         B     CH30                                                             
*                                                                               
         CLI   8(R2),C'A'          TEST ADD                                     
         BNE   *+12                                                             
         BAS   RE,ADDONE           YES - VALIDATE, THEN ADD THE RECORD          
         B     CH30                                                             
*                                                                               
         CLI   SVUSECMP,C'Y'       TEST COMPETITION/ESTIMATE USER               
         BNE   CH10                NO - INVALID INPUT                           
         CLI   8(R2),C'X'          TEST COMPETITION SELECT                      
         BE    CH15                YES - VALIDATE, THEN SELECT                  
         CLI   8(R2),C'E'          TEST ESTIMATE SELECT                         
         BE    CH15                YES - VALIDATE, THEN SELECT                  
*                                                                               
CH10     MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
CH15     ZIC   R4,SQNMDISP                                                      
         LA    R4,SQNMS(R4)        THIS SEQUENCE NUMBER                         
         CLI   0(R4),0             TEST RECORD WAS DELETED                      
         BE    CH30                YES - IGNORE ANY CHANGES                     
*                                                                               
         MVC   SVDETADR,0(R3)      SAVE DETAIL DISK ADDRESS                     
         ST    R2,ASELFLDH         HANG ON TO A(SELECT FIELD)                   
         LA    R1,5                NO. OF FIELDS PER ROW                        
         SR    R0,R0                                                            
CH20     IC    R0,0(R2)                                                         
         AR    R2,R0               BUMP TO DATA FIELD                           
         TM    4(R2),FINPVAL       PREVIOUSLY VALIDATED?                        
         BZ    CH40                NO - FIELD HAS CHANGED                       
         BCT   R1,CH20                                                          
         B     CH230               NO FIELDS WERE CHANGED                       
*                                                                               
CH30     LA    R1,6                NO. OF FIELDS PER ROW                        
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,*-6              BUMP TO NEXT SELECT                          
         B     CH240                                                            
*                                                                               
CH40     MVC   AIO,AIO2            USE IO2 FOR DETAIL RECORDS                   
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),SYSFIL,(R3),AIO,DMWORK           
         CLI   8(R1),0             GET THE DETAIL RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,ASELFLDH         A(SELECT FIELD)                              
         ZIC   R0,0(R2)            BUMP TO DAY FIELD                            
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R3),8(R2)),DAY,WORK                                
         CLI   DAY,0                                                            
         BNE   *+12                                                             
         MVI   ERROR,INVDAY                                                     
         B     TRAPERR                                                          
*                                                                               
         ZIC   R0,0(R2)            BUMP TO TIME FIELD                           
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R3),8(R2)),TIME                                    
         MVI   ERROR,INVTIME                                                    
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
         CLC   =C'NONE',TIME                                                    
         BE    TRAPERR                                                          
         CLC   =C'VARY',TIME                                                    
         BE    TRAPERR                                                          
*                                                                               
         ZIC   R4,SQNMDISP                                                      
         LA    R4,SQNMS(R4)        THE OLD SEQUENCE NUMBER                      
         L     R5,AIO1             NSID RECORD                                  
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDPELEM,R5                                                       
CH50     CLC   DAYTIME,EDPDAY      LOOK FOR THE GIVEN DAY/TIME                  
         BE    *+16                                                             
         BAS   RE,NEXTEL           KEEP LOOKING FOR DAY/TIME                    
         BE    CH50                                                             
         B     CH60                                                             
*                                                                               
         CLC   EDPSEQ,0(R4)        TEST SAME AS NEW                             
         BNE   DUPSEQ              DAY/TIME ALREADY EXISTS ELSEWHERE            
         B     CH80                                                             
*                                                                               
CH60     L     R5,AIO1                                                          
         BAS   RE,GETEL            LOOK AT DAY/TIME ELEMENTS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CH70     CLC   EDPSEQ,0(R4)        FIND THE ELEMENT TO BE CHANGED               
         BE    *+14                                                             
         BAS   RE,NEXTEL                                                        
         BE    CH70                                                             
         DC    H'0'                                                             
*                                                                               
         MVC   EDPDAY(5),DAYTIME   CHANGE THE DAY/TIME                          
         DROP  R5                                                               
*                                                                               
CH80     ZIC   R0,0(R2)            BUMP TO PROGRAM TYPE                         
         AR    R2,R0                                                            
         MVI   PROGTYPE,0          ASSUME PROGRAM TYPE WAS DELETED              
         CLI   5(R2),0                                                          
         BE    CH110               IT WAS                                       
*                                                                               
         LA    R3,PRGTYPSV         LOOK FOR MATCH IN LIST                       
CH90     CLC   0(1,R3),8(R2)                                                    
         BE    CH100                                                            
         LA    R3,8(R3)                                                         
         CLI   0(R3),0             TEST END OF TABLE                            
         BNE   CH90                NO                                           
         MVI   ERROR,INVPRGTP                                                   
         B     TRAPERR                                                          
*                                                                               
CH100    MVC   PROGTYPE,8(R2)      PUT PROGTYP CODE IN ELEMENT                  
*                                                                               
CH110    L     R5,AIO                                                           
         MVI   ELCODE,EDPCODEQ     DAY/TIME/PROGTYP ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDPELEM,R5                                                       
         MVC   EDPDAY(5),DAYTIME   SAVE THE DAY/TIME                            
         MVC   EDPPROG,PROGTYPE    SAVE THE NEW PROGRAM TYPE                    
*                                                                               
         L     R5,AIO1             NSID RECORD                                  
         BAS   RE,GETEL            LOOK AT DAY/TIME ELEMENTS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CH120    CLC   EDPSEQ,0(R4)        FIND THE ELEMENT TO BE CHANGED               
         BE    *+14                                                             
         BAS   RE,NEXTEL                                                        
         BE    CH120                                                            
         DC    H'0'                                                             
         MVC   EDPPROG,PROGTYPE    CHANGE THE PROGRAM TYPE                      
         DROP  R5                                                               
*                                                                               
         MVI   ELCODE,EECCODEQ     EFFECTIVE COST ELEMENT                       
         ZIC   R0,0(R2)            BUMP TO COST                                 
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST FIELD IS EMPTY                          
         BNE   CH130               NO                                           
*                                                                               
         L     R5,AIO                                                           
         BAS   RE,GETEL            TEST THEY ARE TRYING TO DELETE COST          
         BE    COSTDEL             YES - ERROR                                  
         B     CH140               NO - CONTINUE                                
*                                                                               
CH130    ZIC   R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(C'N',8(R2)),(R4)                                   
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         L     R1,4(R1)                                                         
         MH    R1,=H'100'                                                       
         STCM  R1,15,COST                                                       
*                                                                               
         L     R5,AIO                                                           
         USING EECELEM,R5                                                       
         BAS   RE,GETEL            EFFECTIVE COST ELEMENT                       
         BNE   *+14                                                             
         MVC   EECCOST1,COST       SAVE NEW COST VALUE                          
         B     CH140                                                            
*                                                                               
         LA    R5,ELEM             CREATE 30'S COST ELEMENT                     
         XC    ELEM,ELEM                                                        
         MVI   EECCODE,EECCODEQ                                                 
         MVI   EECLEN,EECLENEQ                                                  
         MVI   EECINDEX,1                                                       
         MVI   EECSLN,30                                                        
         MVC   EECCOST1,COST                                                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
CH140    ZIC   R0,0(R2)            BUMP TO UPG/DEMO/PROG FIELD                  
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST FIELD WAS DELETED                       
         BNE   CH160               IT WAS NOT                                   
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EUPCODEQ     UPGRADE ELEMENT                              
         BAS   RE,GETEL            SEE IF IT EXISTS                             
         BNE   CH150               NO                                           
         GOTO1 REMELEM             IT DOES, SO DELETE IT                        
         MVI   ELCODE,EOVCODEQ     OVERRIDE ELEMENT                             
         GOTO1 REMELEM             DELETE OVERRIDE AS WELL                      
         B     CH200                                                            
*                                                                               
CH150    CLI   SVUSECMP,C'Y'       TEST COMPETITION USER                        
         BE    CH200                                                            
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EDOCODEQ     OVERRIDE DEMO VALUES                         
         BAS   RE,GETEL            SEE IF IT EXISTS                             
         BNE   CH155               NO                                           
         GOTO1 REMELEM             DELETE THIS ELEMENT                          
         B     CH200                                                            
*                                                                               
CH155    MVI   ELCODE,EPRCODEQ     PROGRAMMING ELEMENT                          
         GOTO1 REMELEM             DELETE THIS ELEMENT                          
         B     CH200                                                            
*                                                                               
CH160    CLC   =C'* UPGRADE TOO LONG *',8(R2)                                   
         BE    CH200                                                            
         CLC   =C'* OVERRIDE TOO LONG *',8(R2)                                  
         BE    CH200                                                            
*                                                                               
         CLI   SVUSECMP,C'Y'       TEST COMPETITION USER                        
         BE    CH180               YES - ONLY UPGRADES ARE ALLOWED HERE         
         CLC   =C'UPT=',8(R2)      TEST AN UPGRADE IS BEING GIVEN               
         BE    CH180               YES                                          
         CLC   =C'PROG=',8(R2)     TEST PROGRAMMING IS GIVEN                    
         BE    CH195               YES                                          
*                                                                               
         MVI   ELCODE,EDOCODEQ     DEMO OVERRIDE VALUES                         
         GOTO1 REMELEM             DELETE THIS ELEMENT                          
*                                                                               
         LA    RE,BLOCK            SET UP FOR DEMOVAL                           
         LA    RF,480                                                           
         XCEF                                                                   
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
         SR    R3,R3               FIELD ERROR NUMBER                           
         LA    R4,BLOCK+256        R4 - SCAN OUT AREA                           
         GOTO1 SCANNER,DMCB,(R2),(R4)                                           
*                                                                               
CH170    LA    R3,1(R3)            INCREMENT FIELD ERROR NUMBER                 
         XC    DEMWRK(40),DEMWRK                                                
         ZIC   R1,0(R4)            L'DEMO NAME                                  
         STC   R1,DEMWRK+5                                                      
         LA    R1,8(R1)            L'DEMONAME + HEADER                          
         STC   R1,DEMWRK                                                        
         MVC   DEMWRK+8(10),12(R4) DEMNAME TO DEMWRK                            
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         GOTO1 CDEMOVAL,DMCB,(1,DEMWRK),DEMWRK+20,(C'S',BLOCK)                  
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               TEST DEMO ERROR                              
         BZ    DEMOERR                                                          
         DROP  R5                                                               
*                                                                               
         CLI   1(R4),0             L'2ND SCAN OUTFIELD (DEMVALUE)               
         BE    RTNGERR                                                          
         ZIC   R5,1(R4)                                                         
         GOTO1 CASHVAL,DMCB,(1,22(R4)),(R5)                                     
         CLI   0(R1),X'FF'                                                      
         BE    RTNGERR                                                          
         CLC   4(4,R1),=F'32678'   MAX CASHVALUE IN HALF WRD                    
         BH    RTNGERR                                                          
*                                                                               
         LA    R5,ELEM                                                          
         USING EDOELEM,R5                                                       
         XC    ELEM,ELEM                                                        
         MVI   EDOCODE,EDOCODEQ                                                 
         MVI   EDOLEN,EDOLENEQ                                                  
         MVC   EDODEMO,DEMWRK+21   LAST 2 BYTES OF 3 BYTE CODE                  
         MVC   EDOVALUE,6(R1)      AFTER CASHVAL,2ND PARM=VALUE                 
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R4,32(R4)           INCREMENT POSITION IN BLOCK                  
         OC    0(7,R4),0(R4)                                                    
         BNZ   CH170                                                            
         B     CH200                                                            
         DROP  R5                                                               
*                                                                               
CH180    MVI   ELCODE,EUPCODEQ     UPGRADE ELEMENT                              
         GOTO1 REMELEM                                                          
         MVI   ELCODE,EOVCODEQ     OVERRIDE ELEMENT                             
         GOTO1 REMELEM                                                          
*                                                                               
         GOTO1 VALIUPG,DMCB,BLOCK+256                                           
         OC    CONHEAD,CONHEAD     TEST FOR ERROR MESSAGE                       
         BNZ   SOLONG              YES - LEAVE                                  
         LA    R3,BLOCK+256                                                     
         USING SPDEMUPD,R3                                                      
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EUPELEM,R5                                                       
         MVI   EUPCODE,EUPCODEQ                                                 
         MVI   EUPLEN,EUPLENXQ                                                  
         MVC   EUPUPFIL,SPUPFIL                                                 
         MVC   EUPUPGRD,SPUPTYPE                                                
         MVC   EUPUPFBK,SPUPFBK                                                 
         MVC   EUPUPINP,SPUPPRG                                                 
         MVC   EUPUPMBK(6),SPUPFBKL                                             
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         OC    SPUPUDAY,SPUPUDAY                                                
         BNZ   CH190                                                            
         OC    SPUPUTIM,SPUPUTIM                                                
         BNZ   CH190                                                            
         OC    SPUPSTA,SPUPSTA                                                  
         BZ    CH200                                                            
*                                                                               
CH190    LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EOVELEM,R5                                                       
         MVI   EOVCODE,EOVCODEQ                                                 
         MVI   EOVLEN,EOVLENXQ                                                  
         MVC   EOVUPDAY,SPUPUDAY                                                
         MVC   EOVUPTIM,SPUPUTIM                                                
         MVC   EOVUPSTA,SPUPSTA                                                 
         GOTO1 ADDELEM                                                          
         B     CH200                                                            
         DROP  R3,R5                                                            
*                                                                               
CH195    CLI   5(R2),22            'PROG=' PLUS 17 CHARACTER MAX LEN            
         BH    PROGERR                                                          
         CLI   5(R2),6                                                          
         BL    PROGERR                                                          
*                                                                               
         MVI   ELCODE,EPRCODEQ                                                  
         GOTO1 REMELEM                                                          
         LA    R5,ELEM             BUILD PROGRAMMING ELEMENT                    
         XC    ELEM,ELEM                                                        
         USING EPRELEM,R5                                                       
         MVI   EPRCODE,EPRCODEQ                                                 
         MVI   EPRLEN,EPRLENEQ                                                  
         MVC   EPRTEXT,13(R2)                                                   
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
CH200    ZIC   R3,DSKADDX                                                       
         LA    R3,DSKADD(R3)       WE'RE UP TO THIS D/A                         
         GOTO1 DATAMGR,DMCB,(0,=C'PUTREC'),SYSFIL,(R3),AIO,DMWORK               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MODSCRN,C'Y'        SET SCREEN MODIFIED FLAG                     
*                                                                               
         L     RF,ASELFLDH                                                      
         LA    R1,3                NO. OF FIELDS AFFECTING NSID                 
         SR    R0,R0                (DAY, TIME, PROGTYPE)                       
CH205    IC    R0,0(RF)                                                         
         AR    RF,R0               BUMP TO DATA FIELD                           
         TM    4(RF),FINPVAL       PREVIOUSLY VALIDATED?                        
         BZ    *+12                NO - FIELD HAS CHANGED                       
         BCT   R1,CH205                                                         
         B     CH230               NSID NEED NOT BE CHANGED                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       THE NSID KEY                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3            READ OLD RECORD INTO UNUSED AREA             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1            THE UNSORTED NSID RECORD                     
         L     R5,AIO                                                           
         USING EDPELEM,R5                                                       
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENTS                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R3,R3               COUNT THE ELEMENTS                           
*                                                                               
CH210    XI    EDPDAY,X'FF'        MASSAGE DAY/TIME FOR SORTING                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EDPSTRT                                                     
         CH    R0,=H'600'                                                       
         BNL   *+12                                                             
         AH    R0,=H'2400'                                                      
         STCM  R0,3,EDPSTRT                                                     
*                                                                               
         OC    EDPENDT,EDPENDT                                                  
         BZ    CH215                                                            
         CLC   EDPENDT,=C'CC'                                                   
         BE    CH215                                                            
         ICM   R0,3,EDPENDT                                                     
         CH    R0,=H'600'                                                       
         BNL   CH215                                                            
         AH    R0,=H'2400'                                                      
         STCM  R0,3,EDPENDT                                                     
*                                                                               
CH215    LA    R3,1(R3)            INCREMENT ELEMENT COUNT                      
         BAS   RE,NEXTEL                                                        
         BE    CH210                                                            
*                                                                               
         L     R5,AIO                                                           
         BAS   RE,GETEL            R5 - A(FIRST DAY/TIME)                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 XSORT,DMCB,(R5),(R3),10,5,4                                      
         SR    R3,R3                                                            
*                                                                               
CH220    LA    R3,1(R3)                                                         
         STC   R3,EDPINDEX         SORTING SEQUENCE NO.                         
         XI    EDPDAY,X'FF'        RESTORE DAY CODE                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EDPSTRT        RESTORE START TIME                           
         CH    R0,=H'2400'                                                      
         BNH   *+12                                                             
         SH    R0,=H'2400'                                                      
         STCM  R0,3,EDPSTRT                                                     
*                                                                               
         CLC   EDPENDT,=C'CC'                                                   
         BE    CH225                                                            
         ICM   R0,3,EDPENDT        RESTORE END TIME                             
         CH    R0,=H'2400'                                                      
         BNH   CH225                                                            
         SH    R0,=H'2400'                                                      
         STCM  R0,3,EDPENDT                                                     
*                                                                               
CH225    BAS   RE,NEXTEL                                                        
         BE    CH220                                                            
         GOTO1 PUTREC              SAVE THE SORTED NSID RECORD                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
CH230    L     RF,ASELFLDH         A(SELECT FIELD)                              
         LA    R1,5                5 DATA FIELDS                                
         SR    R0,R0                                                            
         IC    R0,0(RF)            BUMP TO FIRST DATA FIELD                     
         AR    RF,R0                                                            
         OI    4(RF),FINPVAL       MARK FIELD AS VALIDATED                      
         BCT   R1,*-10                                                          
*                                                                               
         L     RF,ASELFLDH                                                      
         CLI   8(RF),C'S'          TEST SELECT WAS REQUESTED                    
         BE    SELDET              DO THE SELECT                                
         CLI   8(RF),C'C'                                                       
         BE    SELDET                                                           
*                                                                               
         CLI   SVUSECMP,C'Y'       TEST COMPETITION/ESTIMATE USER               
         BNE   CH235               NO                                           
*                                                                               
         CLI   8(RF),C'X'          TEST COMPETITION                             
         BE    SELCOMP                                                          
         CLI   8(RF),C'E'          TEST ESTIMATING                              
         BE    SELESTIM                                                         
*                                                                               
CH235    MVC   AIO,AIO1            RESTORE AIO                                  
         ZIC   R0,0(R2)            BUMP TO NEXT SELECT                          
         AR    R2,R0                                                            
*                                                                               
CH240    ZIC   R1,SQNMDISP         BUMP TO NEXT SEQUENCE NO.                    
         LA    R1,1(R1)                                                         
         STC   R1,SQNMDISP                                                      
*                                                                               
         ZIC   R3,DSKADDX          BUMP TO NEXT D/A                             
         LA    R3,4(R3)                                                         
         STC   R3,DSKADDX                                                       
         LA    R3,DSKADD(R3)       R3 - MUST POINT TO NEXT D/A                  
*                                                                               
         OC    0(4,R3),0(R3)       TEST ANY MORE RECORDS                        
         BZ    *+14                NO                                           
         LA    RF,SIRPFH                                                        
         CR    R2,RF               TEST END OF SCREEN                           
         BNE   CH5                 NO - PROCESS NEXT RECORD                     
*                                                                               
         ZIC   R3,SQNMDISP                                                      
         LA    R3,SQNMS(R3)                                                     
         CLI   0(R3),0             TEST ANY ELEMENTS LEFT AT ALL                
         BNE   DR35                YES - DISPLAY NEXT PAGE                      
*                                                                               
         CLI   ACTNUM,ACTSEL       TEST SELECT                                  
         BE    SR                                                               
         MVI   NEWKEY,C'Y'         FORCE LIST FROM BEGINNING                    
         B     DR                                                               
         EJECT                                                                  
* ADD A NEW DAY/TIME                                                            
*                                                                               
ADDONE   NTR1                                                                   
*                                                                               
         ST    R2,ASELFLDH         HANG ON TO A(SELECT FIELD)                   
*                                                                               
         ZIC   R0,0(R2)            BUMP TO DAY FIELD                            
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R3),8(R2)),DAY,WORK                                
         CLI   DAY,0                                                            
         BNE   *+12                                                             
         MVI   ERROR,INVDAY                                                     
         B     TRAPERR                                                          
*                                                                               
         ZIC   R0,0(R2)            BUMP TO TIME FIELD                           
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R3),8(R2)),TIME                                    
         MVI   ERROR,INVTIME                                                    
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
         CLC   =C'NONE',TIME                                                    
         BE    TRAPERR                                                          
         CLC   =C'VARY',TIME                                                    
         BE    TRAPERR                                                          
*                                                                               
         L     R5,AIO              NSID RECORD                                  
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         MVI   SEQNUM,1            FIRST DAY/TIME ELEMENT                       
         B     ADD30                                                            
*                                                                               
         XC    BLOCK(256),BLOCK    CLEAR USED SEQUENCE NUMBER ARRAY             
         USING EDPELEM,R5                                                       
*                                                                               
ADD10    CLC   DAYTIME,EDPDAY      LOOK FOR THE GIVEN DAY/TIME                  
         BE    DUPSEQ              ALREADY EXISTS - ERROR                       
*                                                                               
         ZIC   R0,EDPSEQ                                                        
         LA    R3,BLOCK                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,*-4                                                           
         MVI   0(R3),X'FF'         MARK SEQUENCE NUMBER AS USED                 
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    ADD10                                                            
         DROP  R5                                                               
*                                                                               
         LA    R1,1                                                             
         LA    R3,BLOCK+1          LOOK FOR FIRST AVAILABLE SEQNUM              
ADD20    CLI   0(R3),0                                                          
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         B     ADD20                                                            
         STC   R1,SEQNUM           HERE IT IS                                   
*                                                                               
ADD30    XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING SIRREC,R5                                                        
         L     R4,AIO              NSID RECORD                                  
         MVC   SIRKEY,0(R4)                                                     
         CLI   SEQNUM,0            MAKE SURE THERE'S A SEQUENCE NO.             
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SIRKSEQ,SEQNUM      DETAIL KEY                                   
         MVC   AIO,AIO2            BUILD DETAIL RECORD IN IO2                   
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS AS WELL            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ADD40                                                            
         CLI   DMCB+8,X'02'        TEST KEY WAS DELETED                         
         BE    *+6                 MUST HAVE BEEN                               
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEKEY,KEY         HANG ON TO THE DELETED KEY                   
         GOTO1 GETREC              GET THE DELETED RECORD                       
         CLI   DMCB+8,X'02'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         NI    SIRRCNTL,X'7F'      UNDELETE THE RECORD                          
         MVI   ELCODE,0            LOOK FOR ANY ELEMENT AT ALL                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   0(R5),X'FF'         MARK ALL ELEMENTS FOR DELETION               
         BAS   RE,NEXTEL                                                        
         CLI   0(R5),X'F1'         EXCEPT THE ACTIVITY ELEMENT                  
         BNE   *-12                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             REMOVE ALL ELEMENTS                          
         MVI   ADDDET,C'N'         WE'LL DO PUTREC LATER                        
         B     ADD50                                                            
*                                                                               
ADD40    L     R5,AIO              BUILD NEW RECORD                             
         XC    0(256,R5),0(R5)                                                  
         MVC   SIRKEY,KEYSAVE      THE NEW DETAIL KEY                           
         MVC   SIRRLEN,=H'24'      CURRENT RECORD LENGTH                        
         MVI   ADDDET,C'Y'         WE'LL DO ADDREC LATER                        
         DROP  R5                                                               
*                                                                               
ADD50    NI    DMINBTS,X'F7'       DON'T READ DELETED RECORDS                   
         ZIC   R0,0(R2)            BUMP TO PROGRAM TYPE                         
         AR    R2,R0                                                            
*                                                                               
         MVI   PROGTYPE,0          ASSUME NO PROGRAM TYPE                       
         CLI   5(R2),0             TEST PROGTYPE GIVEN                          
         BE    ADD80               NO                                           
*                                                                               
         LA    R3,PRGTYPSV         LOOK FOR MATCH IN LIST                       
ADD60    CLC   0(1,R3),8(R2)                                                    
         BE    ADD70                                                            
         LA    R3,8(R3)                                                         
         CLI   0(R3),0             TEST END OF TABLE                            
         BNE   ADD60               NO                                           
         MVI   ERROR,INVPRGTP                                                   
         B     TRAPERR                                                          
*                                                                               
ADD70    MVC   PROGTYPE,8(R2)      PUT PROGTYP CODE IN ELEMENT                  
*                                                                               
ADD80    LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EDPELEM,R5                                                       
         MVI   EDPCODE,EDPCODEQ    DAY/TIME/PROGTYP ELEMENT                     
         MVI   EDPLEN,EDPLENEQ     ELEMENT LENGTH                               
         MVC   EDPSEQ,SEQNUM       SEQUENCE NUMBER                              
         MVC   EDPDAY(5),DAYTIME   SAVE THE DAY/TIME                            
         MVC   EDPPROG,PROGTYPE    SAVE THE NEW PROGRAM TYPE                    
         GOTO1 ADDELEM                                                          
*                                                                               
         MVC   AIO,AIO1            NOW ADD ELEMENT TO NSID RECORD               
         MVI   EDPINDEX,1          ASSUME IT'S THE FIRST ONE                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         MVC   AIO,AIO2            BACK TO DETAIL RECORD                        
         ZIC   R0,0(R2)            BUMP TO COST                                 
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST A COST GIVEN                            
         BE    ADD90               NO                                           
*                                                                               
         ZIC   R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(C'N',8(R2)),(R4)                                   
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         L     R1,4(R1)                                                         
         MH    R1,=H'100'                                                       
         STCM  R1,15,COST                                                       
*                                                                               
         USING EECELEM,R5                                                       
         LA    R5,ELEM             CREATE 30'S COST ELEMENT                     
         XC    ELEM,ELEM                                                        
         MVI   EECCODE,EECCODEQ                                                 
         MVI   EECLEN,EECLENEQ                                                  
         MVI   EECINDEX,1                                                       
         MVI   EECSLN,30                                                        
         MVC   EECCOST1,COST                                                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
ADD90    ZIC   R0,0(R2)            BUMP TO UPG/DEMO/PROG FIELD                  
         AR    R2,R0                                                            
         CLI   5(R2),0             TEST ANY DATA                                
         BE    ADD130              NO                                           
*                                                                               
         CLI   SVUSECMP,C'Y'       TEST COMPETITION/ESTIMATE USER               
         BE    ADD110              YES - ONLY UPGRADE IS ALLOWED HERE           
         CLC   =C'UPT=',8(R2)      TEST AN UPGRADE IS BEING GIVEN               
         BE    ADD110              YES                                          
         CLC   =C'PROG=',8(R2)     TEST PROGRAMMING IS BEING GIVEN              
         BE    ADD125              YES                                          
*                                                                               
         LA    RE,BLOCK            SET UP FOR DEMOVAL                           
         LA    RF,480                                                           
         XCEF                                                                   
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
         SR    R3,R3               FIELD NUMBER (FOR ERRORS)                    
         LA    R4,BLOCK+256        R4 - SCAN OUT AREA                           
         GOTO1 SCANNER,DMCB,(R2),(R4)                                           
*                                                                               
ADD100   LA    R3,1(R3)            INCREMENT FIELD ERROR NUMBER                 
         XC    DEMWRK(40),DEMWRK                                                
         ZIC   R1,0(R4)            L'DEMO NAME                                  
         STC   R1,DEMWRK+5                                                      
         LA    R1,8(R1)            L'DEMONAME + HEADER                          
         STC   R1,DEMWRK                                                        
         MVC   DEMWRK+8(10),12(R4) DEMNAME TO DEMWRK                            
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         GOTO1 CDEMOVAL,DMCB,(1,DEMWRK),DEMWRK+20,(C'S',BLOCK)                  
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0               TEST DEMO ERROR                              
         BZ    DEMOERR                                                          
         DROP  R5                                                               
*                                                                               
         CLI   1(R4),0             L'2ND SCAN OUTFIELD (DEMVALUE)               
         BE    RTNGERR                                                          
         ZIC   R5,1(R4)                                                         
         GOTO1 CASHVAL,DMCB,(1,22(R4)),(R5)                                     
         CLI   0(R1),X'FF'                                                      
         BE    RTNGERR                                                          
         CLC   4(4,R1),=F'32678'   MAX CASHVALUE IN HALF WRD                    
         BH    RTNGERR                                                          
*                                                                               
         LA    R5,ELEM                                                          
         USING EDOELEM,R5                                                       
         XC    ELEM,ELEM                                                        
         MVI   EDOCODE,EDOCODEQ                                                 
         MVI   EDOLEN,EDOLENEQ                                                  
         MVC   EDODEMO,DEMWRK+21   LAST 2 BYTES OF 3 BYTE CODE                  
         MVC   EDOVALUE,6(R1)      AFTER CASHVAL,2ND PARM=VALUE                 
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R4,32(R4)           INCREMENT POSITION IN SCANBLK                
         OC    0(7,R4),0(R4)                                                    
         BNZ   ADD100                                                           
         B     ADD130                                                           
         DROP  R5                                                               
*                                                                               
ADD110   GOTO1 VALIUPG,DMCB,BLOCK+256                                           
         OC    CONHEAD,CONHEAD     TEST FOR ERROR MESSAGE                       
         BNZ   SOLONG              YES - LEAVE                                  
         LA    R3,BLOCK+256                                                     
         USING SPDEMUPD,R3                                                      
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EUPELEM,R5                                                       
         MVI   EUPCODE,EUPCODEQ                                                 
         MVI   EUPLEN,EUPLENXQ                                                  
         MVC   EUPUPFIL,SPUPFIL                                                 
         MVC   EUPUPGRD,SPUPTYPE                                                
         MVC   EUPUPFBK,SPUPFBK                                                 
         MVC   EUPUPINP,SPUPPRG                                                 
         MVC   EUPUPMBK(6),SPUPFBKL                                             
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         OC    SPUPUDAY,SPUPUDAY                                                
         BNZ   ADD120                                                           
         OC    SPUPUTIM,SPUPUTIM                                                
         BNZ   ADD120                                                           
         OC    SPUPSTA,SPUPSTA                                                  
         BZ    ADD130                                                           
*                                                                               
ADD120   LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING EOVELEM,R5                                                       
         MVI   EOVCODE,EOVCODEQ                                                 
         MVI   EOVLEN,EOVLENXQ                                                  
         MVC   EOVUPDAY,SPUPUDAY                                                
         MVC   EOVUPTIM,SPUPUTIM                                                
         MVC   EOVUPSTA,SPUPSTA                                                 
         GOTO1 ADDELEM                                                          
         B     ADD130                                                           
         DROP  R3,R5                                                            
*                                                                               
ADD125   CLI   5(R2),22            'PROG=' PLUS 17 CHARACTER MAX LEN            
         BH    PROGERR                                                          
         CLI   5(R2),6                                                          
         BL    PROGERR                                                          
*                                                                               
         LA    R5,ELEM             BUILD PROGRAMMING ELEMENT                    
         XC    ELEM,ELEM                                                        
         USING EPRELEM,R5                                                       
         MVI   EPRCODE,EPRCODEQ                                                 
         MVI   EPRLEN,EPRLENEQ                                                  
         MVC   EPRTEXT,13(R2)                                                   
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
ADD130   ZIC   R3,DSKADDX          TABLE POSITION OF OLD DISK ADDRESS           
         LA    R3,DSKADD(R3)                                                    
         CLI   ADDDET,C'Y'         TEST ADD NEW DETAIL RECORD                   
         BNE   ADD140                                                           
         GOTO1 ADDREC              ADD THE NEW DETAIL RECORD                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,DMCB+8                                                        
         MVC   0(4,R3),0(R1)       PUT NEW DISK ADDRESS IN TABLE                
         B     ADD150                                                           
*                                                                               
ADD140   GOTO1 PUTREC              UNDELETE AND UPDATE NEW RECORD               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(18),SAVEKEY     WE'LL UNDELETE THIS KEY                      
         MVC   0(4,R3),KEY+14      PUT UNDELETED DISK ADDRESS IN TABLE          
         NI    KEY+13,X'7F'        UNDELETE                                     
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADD150   XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       THE NSID KEY                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3            READ INTO USELESS AREA                       
         GOTO1 GETREC              SET DMWORK FOR NSID RECORD                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1            THE UNSORTED NSID RECORD                     
         L     R5,AIO                                                           
         USING EDPELEM,R5                                                       
         MVI   ELCODE,EDPCODEQ     DAY/TIME ELEMENTS                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R3,R3               COUNT THE ELEMENTS                           
*                                                                               
ADD160   XI    EDPDAY,X'FF'        MASSAGE DAY/TIME FOR SORTING                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EDPSTRT                                                     
         CH    R0,=H'600'                                                       
         BNL   *+12                                                             
         AH    R0,=H'2400'                                                      
         STCM  R0,3,EDPSTRT                                                     
*                                                                               
         OC    EDPENDT,EDPENDT                                                  
         BZ    ADD165                                                           
         CLC   EDPENDT,=C'CC'                                                   
         BE    ADD165                                                           
         ICM   R0,3,EDPENDT                                                     
         CH    R0,=H'600'                                                       
         BNL   ADD165                                                           
         AH    R0,=H'2400'                                                      
         STCM  R0,3,EDPENDT                                                     
*                                                                               
ADD165   LA    R3,1(R3)            INCREMENT ELEMENT COUNT                      
         BAS   RE,NEXTEL                                                        
         BE    ADD160                                                           
*                                                                               
         L     R5,AIO                                                           
         BAS   RE,GETEL            R5 - A(FIRST DAY/TIME)                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 XSORT,DMCB,(R5),(R3),10,5,4                                      
         SR    R3,R3                                                            
*                                                                               
ADD170   LA    R3,1(R3)                                                         
         STC   R3,EDPINDEX         SORTING SEQUENCE NO.                         
         XI    EDPDAY,X'FF'        RESTORE DAY CODE                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EDPSTRT        RESTORE START TIME                           
         CH    R0,=H'2400'                                                      
         BNH   *+12                                                             
         SH    R0,=H'2400'                                                      
         STCM  R0,3,EDPSTRT                                                     
*                                                                               
         CLC   EDPENDT,=C'CC'                                                   
         BE    ADD180                                                           
         ICM   R0,3,EDPENDT        RESTORE END TIME                             
         CH    R0,=H'2400'                                                      
         BNH   ADD180                                                           
         SH    R0,=H'2400'                                                      
         STCM  R0,3,EDPENDT                                                     
*                                                                               
ADD180   BAS   RE,NEXTEL                                                        
         BE    ADD170                                                           
         GOTO1 PUTREC              UPDATE THE NSID RECORD                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         ZIC   R1,SQNMDISP                                                      
         LA    R1,SQNMS(R1)                                                     
         MVC   0(1,R1),SEQNUM      PUT NEW SEQ. NO. IN THE LIST                 
*                                                                               
         L     R1,ASELFLDH         A(SELECT FIELD)                              
         MVI   8(R1),C'*'          REMOVE 'A' FROM SELECT FIELD                 
         OI    6(R1),FOUTTRN       XMIT                                         
         MVC   AIO,AIO1            RESTORE AIO                                  
         B     XIT                                                              
         EJECT                                                                  
* DELETE DETAIL RECORD                                                          
*                                                                               
DELDET   NTR1                                                                   
*                                                                               
         MVI   8(R2),C'*'          REPLACE 'D' WITH '*' ON SCREEN               
         OI    6(R2),FOUTTRN       XMIT                                         
*                                                                               
         ZIC   R3,SQNMDISP                                                      
         LA    R3,SQNMS(R3)                                                     
         CLI   0(R3),0             TEST RECORD IS ALREADY DELETED               
         BE    DELX                                                             
*                                                                               
         MVI   0(R3),0             MARK AS DELETED IN SEQUENCE NO. LIST         
         MVC   AIO,AIO2            USE IO2 FOR DETAIL RECORD                    
         ZIC   R3,DSKADDX                                                       
         LA    R3,DSKADD(R3)                                                    
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),SYSFIL,(R3),AIO,DMWORK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         USING SIRREC,R4                                                        
         OI    SIRRCNTL,X'80'      MARK RECORD FOR DELETION                     
         MVC   SEQNUM,SIRKSEQ                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'PUTREC'),SYSFIL,(R3),AIO,DMWORK               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R4)       THE DETAIL KEY                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+13,X'80'        MARK KEY FOR DELETION                        
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             BUILD NSID KEY                               
         MVC   KEY(13),SVKEY                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR NSID RECORD                         
         CLC   KEY(13),KEYSAVE     BETTER BE THERE                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1            USE AIO1 FOR NSID RECORD                     
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EDPCODEQ                                                  
         BAS   RE,GETEL            LOOK FOR DAY/TIME ELEMENTS                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDPELEM,R5                                                       
DEL10    CLC   EDPSEQ,SEQNUM       FIND THE ELEMENT TO BE DELETED               
         BE    *+14                HERE IT IS                                   
         BAS   RE,NEXTEL           TRY AGAIN                                    
         BE    DEL10                                                            
         DC    H'0'                IT'S GOT TO BE THERE                         
         MVI   EDPCODE,X'FF'       MARK ELEMENT FOR DELETION                    
*                                                                               
         L     R5,AIO              BEGINNING OF NSID RECORD                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             DELETE THE ELEMENT                           
*                                                                               
         L     R5,AIO                                                           
         LA    R3,1                COUNT THE SORTING SEQUENCE NOS.              
         MVI   ELCODE,EDPCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DEL30                                                            
*                                                                               
DEL20    STC   R3,EDPINDEX         UPDATE THE SORTING SEQUENCE NOS.             
         BAS   RE,NEXTEL                                                        
         BNE   DEL30                                                            
         LA    R3,1(R3)                                                         
         B     DEL20                                                            
         DROP  R5                                                               
*                                                                               
DEL30    GOTO1 PUTREC              UPDATE THE NSID RECORD                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MODSCRN,C'Y'        SET SCREEN MODIFIED FLAG                     
*                                                                               
DELX     B     XIT                                                              
         EJECT                                                                  
* DISPLAY DETAIL RECORD FOR CHANGE                                              
*                                                                               
SELDET   L     RF,=A(SELECT)       PREPARE FOR SELECT                           
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
         XC    DMCB(24),DMCB       GET DETAIL SCREEN                            
         LA    R3,CONTAGH                                                       
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217BF'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 LOADAPP,DMCB,(X'0F',(RB))                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY COMPETITION RECORD FOR CHANGE                                         
*                                                                               
SELCOMP  L     RF,=A(SELECT)       PREPARE FOR SELECT                           
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
         XC    DMCB(24),DMCB       LOAD COMPETITION SCREEN                      
         LA    R3,CONTAGH                                                       
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217B6'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 LOADAPP,DMCB,(X'16',(RB))                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY ESTIMATING SCREEN FOR CHANGE                                          
*                                                                               
SELESTIM L     RF,=A(SELECT)       PREPARE FOR SELECT                           
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
*                                                                               
         XC    DMCB(24),DMCB       LOAD ESTIMATING SCREEN                       
         LA    R3,CONTAGH                                                       
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217B7'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 LOADAPP,DMCB,(X'17',(RB))                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* COPY NSID (AND DETAIL) RECORDS                                                
*                                                                               
COPY     OC    OLDKEY,OLDKEY       TEST WE HAVE A KEY TO COPY FROM              
         BZ    NOCOPKEY            NO                                           
         CLI   NEWKEY,C'N'         TEST KEY WAS UNCHANGED                       
         BE    SAMECPKY            YES - CAN'T COPY                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),OLDKEY      'FROM' KEY                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST RECORD FOUND                            
         BNE   NOCOPKEY            NO                                           
*                                                                               
         GOTO1 GETREC              NSID IN IO1                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVC   0(13,R5),SVKEY      PUT 'TO' KEY IN RECORD                       
         MVI   ELCODE,EDPCODEQ     LOOK FOR DAY/TIME ELEMENTS                   
         BAS   RE,GETEL                                                         
         BNE   NOCOPELS            NO ELEMENTS TO COPY                          
*                                                                               
         USING EDPELEM,R5                                                       
COPY10   CLI   EDPPROG,0           TEST ANY PROGTYPE IN DETAIL RECORD           
         BE    COPY30              NO                                           
         CLI   PROGTYP,0           TEST FILTERING ON PROGTYPE                   
         BNE   COPY40                                                           
*                                                                               
         LA    R3,PRGTYPSV                                                      
COPY20   CLC   EDPPROG,0(R3)       PROGTYPES MUST EXIST IN NEW SCHEME           
         BE    COPY30                                                           
         LA    R3,8(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   COPY20                                                           
*                                                                               
         LA    R2,SIRSCHH          PROGTYPE NOT FOUND                           
         MVC   BYTE,EDPPROG                                                     
         B     NEEDPRGT                                                         
*                                                                               
COPY30   CLI   PROGTYP,0           TEST FILTERING ON PROGTYPE                   
         BE    *+18                                                             
*                                                                               
COPY40   CLC   PROGTYP,EDPPROG     TEST SAME PROGTYPE                           
         BE    *+8                                                              
         MVI   EDPCODE,X'FF'       MARK ELEMENT FOR DELETION                    
*                                                                               
         BAS   RE,NEXTEL           TEST ANY MORE TO CHECK                       
         BE    COPY10              YES                                          
*                                                                               
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             FILTER ON PROGTYPE                           
         L     R5,AIO              NO - RESTORE GETEL POINTER                   
         MVI   ELCODE,EDPCODEQ     NOW LOOK FOR DAY/TIME ELEMENTS               
         BAS   RE,GETEL                                                         
         BNE   NOCOPELS            NO ELEMENTS REMAIN TO COPY                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       THE NEW NSID KEY                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST IT'S THERE                              
         BE    COPY60              YES                                          
*                                                                               
         GOTO1 ADDREC              ADD NEW NSID RECORD                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPY50   MVC   AIO,AIO2                                                         
         L     R4,AIO              USE IO2 FOR NEW DETAIL RECORDS               
         USING SIRREC,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   SIRKEY,SVKEY        NSID KEY                                     
         MVC   SIRKSEQ,EDPSEQ      WITH SEQUENCE NUMBER                         
         MVC   SIRRLEN,=H'24'                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,EDPLEN           THE DAY/TIME ELEMENT                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R5)                                                    
         LA    R3,ELEM             NO INDEX NO. IN DETAIL RECORDS               
         MVI   EDPINDEX-EDPELEM(R3),0                                           
         GOTO1 ADDELEM             ADD ELEMENT TO DETAIL RECORD                 
*                                                                               
         GOTO1 ADDREC              ADD NEW DETAIL RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,EDPCODEQ                                                  
         BAS   RE,NEXTEL           LOOK FOR MORE DAY/TIME ELEMENTS              
         BE    COPY50                                                           
         B     COPYX                                                            
*                                                                               
COPY60   MVC   AIO,AIO2                                                         
         GOTO1 GETREC              EXISTING NSID RECORD IN IO2                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EDPCODEQ     LOOK FOR DAY/TIME ELEMENTS                   
         BAS   RE,GETEL                                                         
         BNE   *+12                THERE ARE NONE                               
         MVI   ERROR,RECEXIST      CAN'T OVERWRITE EXISTING RECORD              
         B     TRAPERR                                                          
*                                                                               
         L     R5,AIO1             BACK TO ORIGINAL NSID                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPY70   XC    ELEM,ELEM           DAY/TIME ELEMENT                             
         ZIC   R1,EDPLEN           THE DAY/TIME ELEMENT                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R5)                                                    
         GOTO1 ADDELEM             ADD TO RECORD                                
         BAS   RE,NEXTEL                                                        
         BE    COPY70                                                           
*                                                                               
         L     R5,AIO              BEGINNING OF UPDATED RECORD                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,1                ELEMENT SEQUENCE NOS.                        
COPY80   STC   R3,EDPINDEX                                                      
         LA    R3,1(R3)                                                         
         BAS   RE,NEXTEL                                                        
         BE    COPY80                                                           
         GOTO1 PUTREC              UPDATE EXISTING NSID RECORD                  
*                                                                               
         L     R5,AIO              BEGINNING OF UPDATED RECORD                  
         MVI   ELCODE,EDPCODEQ     LOOK FOR DAY/TIME ELEMENTS                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            NOW USE AIO1 FOR DETAIL RECORDS              
         OI    DMINBTS,X'08'       DETAIL MIGHT HAVE BEEN DELETED               
*                                                                               
COPY90   LA    R4,KEY              BUILD EACH DETAIL KEY                        
         XC    KEY,KEY                                                          
         MVC   SIRKEY,SVKEY                                                     
         MVC   SIRKSEQ,EDPSEQ                                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   COPY100             IT'S NOT THERE                               
         TM    DMCB+8,X'02'        TEST KEY WAS DELETED                         
         BO    *+6                                                              
         DC    H'0'                IT MUST HAVE BEEN                            
*                                                                               
         NI    KEY+13,X'7F'        UNDELETE THE KEY                             
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              DETAIL RECORD IN IO1                         
         TM    DMCB+8,X'02'        TEST RECORD WAS DELETED                      
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         NI    SIRRCNTL,X'7F'      UNDELETE THE RECORD                          
         MVC   SIRRLEN,=H'24'      KILLS ALL THE ELEMENTS                       
         XC    SIRREC+24(2),SIRREC+24                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,EDPLEN           THE DAY/TIME ELEMENT                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R5)                                                    
         LA    R3,ELEM             NO INDEX NO. IN DETAIL RECORDS               
         MVI   EDPINDEX-EDPELEM(R3),0                                           
         GOTO1 ADDELEM             ADD ELEMENT TO DETAIL RECORD                 
         GOTO1 PUTREC                                                           
         B     COPY110                                                          
*                                                                               
COPY100  L     R4,AIO              NEW DETAIL RECORD IN IO1                     
         XC    0(256,R4),0(R4)                                                  
         MVC   SIRKEY,KEYSAVE      NSID KEY                                     
         MVC   SIRRLEN,=H'24'                                                   
         DROP  R4                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,EDPLEN           THE DAY/TIME ELEMENT                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R5)                                                    
         LA    R3,ELEM             NO INDEX NO. IN DETAIL RECORDS               
         MVI   EDPINDEX-EDPELEM(R3),0                                           
         GOTO1 ADDELEM             ADD ELEMENT TO DETAIL RECORD                 
         DROP  R5                                                               
*                                                                               
         GOTO1 ADDREC              ADD NEW DETAIL RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPY110  MVI   ELCODE,EDPCODEQ                                                  
         BAS   RE,NEXTEL           LOOK FOR MORE DAY/TIME ELEMENTS              
         BE    COPY90                                                           
*                                                                               
COPYX    MVC   AIO,AIO1            RESTORE AIO                                  
         NI    DMINBTS,X'F7'       DON'T READ DELETED RECORDS                   
         B     DR                  DISPLAY THE RECORD                           
         EJECT                                                                  
* SELECT NSID RECORD                                                            
*                                                                               
SR       MVI   NOINVFLG,C'N'       RESET NO INVENTORY FLAG                      
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
         LA    R3,LISTKEYS         NSID KEY TABLE                               
SR10     CLI   18(R3),C'S'         TEST RECORD MARKED FOR SELECTION             
         BE    SR20                YES                                          
         LA    R3,19(R3)           NO - BUMP TABLE POINTER                      
         CLI   0(R3),0             TEST END OF TABLE                            
         BNE   SR10                NO                                           
*                                                                               
         XC    DMCB(24),DMCB       YES - GET NSID LIST SCREEN                   
         LA    R3,CONTAGH                                                       
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217AE'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'NSID    '                                           
         MVC   CONACT(8),=C'LIST    '                                           
         MVI   ACTNUM,ACTLIST                                                   
         MVC   SRLLAST+1(2),=X'0101' XMIT ENTIRE SCREEN                         
         MVC   SAVEKEY(13),LISTKEYK                                             
         BAS   RE,DK               DISPLAY THE LIST KEY                         
         BAS   RE,VK               VALIDATE                                     
         L     RF,=A(LISTNSID)     DISPLAY NSID LIST                            
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     XIT                                                              
*                                                                               
SR20     MVI   18(R3),0            A SELECTION WAS MADE                         
         MVC   SAVEKEY(13),0(R3)   FOR DISPKEY ROUTINE                          
         BAS   RE,DK               DISPLAY THE KEY                              
*                                                                               
         NI    SIRSTAH+4,X'FF'-FINPVAL  KEY IS NOT YET VALIDATED                
         OI    SIRSTAH+6,FOUTTRN   XMIT KEY FIELDS                              
         NI    SIRDPTH+4,X'FF'-FINPVAL                                          
         OI    SIRDPTH+6,FOUTTRN                                                
         NI    SIRPERH+4,X'FF'-FINPVAL                                          
         OI    SIRPERH+6,FOUTTRN                                                
         OI    SIRDPERH+6,FOUTTRN                                               
         NI    SIRYRH+4,X'FF'-FINPVAL                                           
         OI    SIRYRH+6,FOUTTRN                                                 
         NI    SIRSCHH+4,X'FF'-FINPVAL                                          
         OI    SIRSCHH+6,FOUTTRN                                                
*                                                                               
         BAS   RE,VK               VALIDATE THE KEY                             
         B     DR                  DISPLAY THE RECORD                           
         EJECT                                                                  
* DISPLAY KEY (LIST AND MAINTENANCE)                                            
*                                                                               
DK       NTR1                                                                   
*                                                                               
         USING SIRREC,R4                                                        
         LA    R4,SAVEKEY          KEY TO BE DISPLAYED                          
*                                                                               
         OC    SIRKMS,SIRKMS       TEST ANY MKT/STA                             
         BZ    DK20                                                             
         GOTO1 MSUNPK,DMCB,(X'80',SIRKMS),WORK,WORK+4                           
         OC    SIRKSTA,SIRKSTA     TEST ANY STATION                             
         BNZ   DK10                YES                                          
         MVC   SIRSTA(4),WORK      MARKET NUMBER                                
         MVC   SIRSTA+4(4),=C'    '                                             
         MVI   SIRSTAH+5,4         INPUT LENGTH 4                               
         OI    SIRSTAH+4,FINPNUM   MAKE NUMERIC                                 
         B     DK20                                                             
*                                                                               
DK10     MVC   SIRSTA,WORK+4       STATION CALL LETTERS                         
         CLI   SIRSTA,X'F0'        CABLE HEADEND?                               
         BL    *+8                                                              
         MVI   SIRSTA+4,C'/'       YES                                          
         LA    R2,SIRSTAH                                                       
         L     RF,=A(FUDGEINP)     PUT INPUT LENGTH INTO FIELD HEADER           
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
DK20     CLI   SIRKDPT,0           TEST DAYPART GIVEN                           
         BE    *+14                                                             
         MVC   SIRDPT,SIRKDPT      DAYPART                                      
         MVI   SIRDPTH+5,1                                                      
*                                                                               
         CLI   SIRKMON,0           TEST PERIOD GIVEN                            
         BE    DK50                                                             
         XC    SIRPER,SIRPER                                                    
         XC    SIRDPER,SIRDPER                                                  
         MVC   PERNUM,SIRKMON      HANG ON TO PERIOD NUMBER                     
         NI    PERNUM,X'7F'        TURN OFF PERIOD INDICATOR BIT                
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY             BUILD SCHEME KEY                             
         MVC   KEY(13),SAVEKEY                                                  
         XC    SIRKMS(9),SIRKMS                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR SCHEME KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3            USE IO3 FOR SCHEME RECORD                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EPNCODEQ     LOOK AT PERIOD NAMES ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,2(R5)            GO PAST OVERHEAD                             
DK30     CLC   PERNUM,0(R5)        TEST MATCH ON PERIOD                         
         BE    *+12                                                             
         LA    R5,5(R5)            NEXT PERIOD                                  
         B     DK30                                                             
*                                                                               
         MVC   SIRPER,1(R5)        PERIOD NAME                                  
         MVI   SIRPERH+5,4         ASSUME INPUT LENGTH OF 4                     
         CLI   SIRPER+3,C' '       IS IT REALLY                                 
         BH    *+8                 YES                                          
         MVI   SIRPERH+5,3         NO - IT'S 3                                  
*                                                                               
         XC    KEY,KEY             BUILD PERIOD KEY                             
         MVC   KEY(13),SAVEKEY                                                  
         XC    SIRKMS,SIRKMS                                                    
         MVI   SIRKDPT,0                                                        
         MVI   SIRKMON,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR PERIOD KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              PERIOD RECORD IN IO3                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         MVI   ELCODE,EPDCODEQ     LOOK FOR PERIOD DEFINITION ELEMENT           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R5                                                       
DK40     CLC   PERNUM,EPDNUM       TEST MATCH ON PERIOD NUMBER                  
         BE    *+14                                                             
         BAS   RE,NEXTEL                                                        
         BE    DK40                                                             
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,EPDSTART),(5,SIRDPER)                             
         GOTO1 DATCON,DMCB,(3,EPDEND),(5,SIRDPER+9)                             
         MVI   SIRDPER+8,C'-'                                                   
         DROP  R5                                                               
*                                                                               
DK50     MVC   BYTE,SIRKYEAR       YEAR IN 1'S COMPLEMENT                       
         XI    BYTE,X'FF'                                                       
         MVC   FULL(1),CURYEAR                                                  
         MVC   FULL+1(2),=X'0101'  PRETEND IT'S JAN01                           
         GOTO1 DATCON,DMCB,(3,FULL),(20,DUB)                                    
         MVC   SIRYR,DUB+2         PRINTABLE YY                                 
         MVI   SIRYRH+5,2                                                       
         OI    SIRYRH+4,FINPNUM    MAKE NUMERIC                                 
*                                                                               
         OC    SIRKCODE,SIRKCODE   TEST SCHEME 'ALL'                            
         BNZ   *+18                NO                                           
         MVC   SIRSCH,=C'ALL'                                                   
         MVI   SIRSCHH+5,3                                                      
         B     DK60                                                             
*                                                                               
         GOTO1 CLUNPK,DMCB,SIRKCODE,SIRSCH                                      
         MVI   SIRSCHH+5,3         ASSUME INPUT LENGTH OF 3                     
         CLI   SIRSCH+2,C' '       IS IT REALLY                                 
         BH    *+8                 YES                                          
         MVI   SIRSCHH+5,2         NO - IT'S 2                                  
*                                                                               
DK60     MVC   AIO,AIO1            RESTORE AIO                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* LIST NSID RECORDS                                                             
*                                                                               
LR       XC    KEY,KEY                                                          
         CLI   NEWKEY,C'Y'         TEST NEW KEY WAS GIVEN                       
         BNE   LR5                 NO                                           
*                                                                               
         MVC   KEY(13),SVKEY       YES - USE THE NEW KEY                        
         MVC   LISTKEYK,SVKEY      HANG ON TO THE NEW KEY                       
*                                                                               
         CLC   SRLSCRN,=C'AE'      TEST NSID LIST SCREEN IS LOADED              
         BNE   LR3                 NO - GO GET IT                               
         L     RF,=A(LISTNSID)     YES - CONTINUE NSID LIST                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(1,(RC))                                               
         B     XIT                                                              
*                                                                               
LR3      XC    DMCB(24),DMCB                                                    
         LA    R3,CONTAGH                                                       
         ST    R3,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217AE'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'NSID    '                                           
         MVC   CONACT(8),=C'LIST    '                                           
         MVI   ACTNUM,ACTLIST                                                   
         MVC   SRLLAST+1(2),=X'0101' XMIT ENTIRE SCREEN                         
         MVC   SAVEKEY(13),SVKEY                                                
         BAS   RE,DK               DISPLAY THE LIST KEY                         
         BAS   RE,VK               VALIDATE THE KEY                             
         L     RF,=A(LISTNSID)     DISPLAY NSID LIST                            
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(1,(RC))                                               
         B     XIT                                                              
*                                                                               
LR5      LA    R3,LISTKEYS         KEYS OF NSID RECORDS                         
         LA    R2,SRLSELH          FIRST SELECT FIELD                           
         MVI   BYTE,0              SELECT MODE FLAG                             
*                                                                               
LR10     CLI   5(R2),0             TEST INPUT IN SELECT FIELD                   
         BE    LR20                NO                                           
         CLI   8(R2),C'S'          TEST SELECT REQUESTED                        
         BE    *+20                YES                                          
         CLI   8(R2),C'C'                                                       
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVI   18(R3),C'S'         MARK D/A AS SELECTED                         
         MVI   BYTE,X'FF'                                                       
*                                                                               
LR20     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)            BUMP TO NEXT SELECT                          
         AR    R2,R0                                                            
         LA    R3,19(R3)           BUMP TO NEXT TABLE POSITION                  
         CLI   0(R2),0             TEST END OF SCREEN                           
         BNE   LR10                NO                                           
*                                                                               
         CLI   BYTE,X'FF'          TEST GO INTO SELECT MODE                     
         BE    LR25                YES                                          
         L     RF,=A(LISTNSID)     NO - CONTINUE NSID LIST                      
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         B     XIT                                                              
*                                                                               
LR25     XC    DMCB(24),DMCB       GET NSID MAINTENANCE SCREEN                  
         LA    R1,CONTAGH                                                       
         ST    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D90217BE'                                           
         CLI   ALTPROG,0           ALTERNATE PHASE FOR SCREENS                  
         BE    *+10                                                             
         MVC   DMCB+6(1),ALTPROG                                                
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'NSID    '                                           
         MVC   CONACT(8),=C'SELECT  '                                           
         MVI   ACTNUM,ACTSEL       WE ARE NOW IN SELECT MODE                    
         MVC   SIRLAST+1(2),=X'0101' XMIT ENTIRE SCREEN                         
         B     SR                                                               
         EJECT                                                                  
         GETEL R5,DATADISP,ELCODE                                               
*                                                                               
RELO     DS    F                   RELOCATION CONSTANT                          
*                                                                               
RTNGERR  CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RTNGERRM),RTNGERRM                                     
         UNPK  CONHEAD+31(2),DUB                                                
         B     SOLONG                                                           
RTNGERRM DC    C'* ERROR * Value error in field    *'                           
*                                                                               
DEMOERR  CVD   R3,DUB              YES - ERROR                                  
         OI    DUB+7,X'0F'                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DEMOERRM),DEMOERRM                                     
         UNPK  CONHEAD+30(2),DUB                                                
         B     SOLONG                                                           
DEMOERRM DC    C'* ERROR * Demo error in field    *'                            
*                                                                               
NEEDPRGT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NEEDPRGM),NEEDPRGM                                     
         MVC   CONHEAD+23(1),BYTE                                               
         B     SOLONG                                                           
NEEDPRGM DC    C'* ERROR * Program type   not defined in new scheme *'          
*                                                                               
DUPSEQ   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DUPSEQM),DUPSEQM                                       
         B     SOLONG                                                           
DUPSEQM  DC    C'* ERROR * Day/time already exists *'                           
*                                                                               
NOCOPKEY XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCOPKEM),NOCOPKEM                                     
         B     SOLONG                                                           
NOCOPKEM DC    C'* ERROR * No record from which to copy *'                      
*                                                                               
NOCOPELS XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCOPELM),NOCOPELM                                     
         B     SOLONG                                                           
NOCOPELM DC    C'* ERROR * No day/times in copied record *'                     
*                                                                               
SAMECPKY XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SAMECPKM),SAMECPKM                                     
         B     SOLONG                                                           
SAMECPKM DC    C'* ERROR * New key must be given *'                             
*                                                                               
NOPGHERE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPGHERM),NOPGHERM                                     
         B     SOLONG                                                           
NOPGHERM DC    C'* ERROR * No program type for action add *'                    
*                                                                               
PROGERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PROGERRM),PROGERRM                                     
         B     SOLONG                                                           
PROGERRM DC    C'* ERROR * Invalid programming text *'                          
*                                                                               
COSTDEL  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'COSTDELM),COSTDELM                                     
         B     SOLONG                                                           
COSTDELM DC    C'* ERROR * Cost may not be deleted *'                           
*                                                                               
NODETAIL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODETALM),NODETALM                                     
         LA    R2,SIRSTAH                                                       
         B     SOLONG                                                           
NODETALM DC    C'* ERROR * DETAIL RECORD MISSING *'                             
*                                                                               
TRAPERR  GOTO1 ERREX               IN SLAVE MODE, GENCON RETURNS                
         B     BYE                                                              
*                                                                               
SOLONG   GOTO1 ERREX2              IN SLAVE MODE, GENCON RETURNS                
BYE      OI    6(R2),FOUTCUR       POSITION CURSOR                              
         OI    CONHEADH+6,FOUTTRN  XMIT HEADER                                  
         GOTO1 SAVEUWKA            SAVE SYSD                                    
         L     RD,SAVERD           BACK OUT ALL THE WAY                         
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* LIST NSID RECORDS                                                             
*                                                                               
LISTNSID DS    0H                                                               
*                                                                               
         NMOD1 0,**LIST**                                                       
         L     RC,0(R1)                                                         
         CLI   0(R1),1             TEST WHERE TO BEGIN                          
         BE    LR40                                                             
*                                                                               
         MVC   KEY(13),SVKEY       ASSUME WE'LL START OVER                      
         OC    LASTKEY,LASTKEY     TEST START LIST FROM BEGINNING               
         BZ    LR40                YES                                          
         MVC   KEY(13),LASTKEY     NO - USE LAST KEY                            
*                                                                               
LR40     LA    R2,SRLSELH          FIRST SELECT FIELD                           
LR50     ZIC   R1,0(R2)            R1 HAS LENGTH OF FIELD                       
         SH    R1,=H'9'            SUBTRACT HEADER LENGTH +1                    
         TM    1(R2),FATBXHDR      TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
*                                                                               
         EX    R1,*+8              PAD WITH BLANKS                              
         B     *+10                                                             
         OC    8(0,R2),=80C' '                                                  
         EX    R1,*+8              TEST FIELD EMPTY                             
         B     *+10                                                             
         CLC   8(0,R2),=80C' '                                                  
         BE    LR60                YES                                          
         EX    R1,*+8              NO - CLEAR FIELD                             
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),FOUTTRN       XMIT                                         
*                                                                               
LR60     ZIC   R0,0(R2)            BUMP                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST END OF SCREEN                           
         BNE   LR50                NO - DO NEXT FIELD                           
*                                                                               
         USING SIRKEY,R4                                                        
         LA    R4,KEY                                                           
         MVC   SAVEKEY(13),KEY     SAVE NSID KEY                                
         XC    SIRKMS(9),SIRKMS                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR SCHEME KEY                          
         CLC   SIRKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2            USE IO2 FOR SCHEME RECORD                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             RESTORE NSID KEY                             
         MVC   SIRKEY,SAVEKEY                                                   
         XC    CONHEAD,CONHEAD     WE'LL PUT A MESSAGE HERE                     
         LA    R2,SRLSELH          BACK TO FIRST SELECT FIELD                   
         LA    RE,LISTKEYS         CLEAR TABLE OF NSID KEYS                     
         LA    RF,323                                                           
         XCEF                                                                   
         LA    R3,LISTKEYS                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
         B     LR80                                                             
*                                                                               
LR70     MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
LR80     CLC   KEY(4),KEYSAVE      TEST SAME AGY/MED/SCHEME                     
         BE    LR90                YES                                          
         XC    LASTKEY,LASTKEY     NO - START AGAIN NEXT TIME                   
         MVC   CONHEAD(43),=C'End of list - select or hit enter for fir+        
               st'                                                              
         B     LRX                                                              
*                                                                               
LR90     OC    SIRKMS,SIRKMS       TEST THERE'S A MKT OR STA                    
         BZ    LR70                                                             
         CLI   SIRKDPT,0           TEST THERE'S A DAYPART                       
         BE    LR70                                                             
         CLI   SIRKSEQ,0           TEST THERE'S NO SEQUENCE NO.                 
         BNE   LR70                                                             
*                                                                               
         OC    BMKT,BMKT           TEST MARKET WAS GIVEN                        
         BZ    *+14                                                             
         CLC   SIRKMKT,BMKT        TEST SAME MARKET                             
         BNE   LR70                                                             
*                                                                               
         OC    BSTA,BSTA           TEST STATION WAS GIVEN                       
         BZ    *+14                                                             
         CLC   SIRKSTA,BSTA        TEST SAME STATION                            
         BNE   LR70                                                             
*                                                                               
         CLI   DAYPART,0           TEST DAYPART WAS GIVEN                       
         BE    *+14                                                             
         CLC   SIRKDPT,DAYPART     TEST SAME DAYPART                            
         BNE   LR70                                                             
*                                                                               
         CLI   PERNUM,0            TEST PERIOD WAS GIVEN                        
         BE    LR100                                                            
         MVC   BYTE,PERNUM                                                      
         OI    BYTE,SIRKBUYQ                                                    
         CLC   BYTE,SIRKMON        TEST SAME PERIOD                             
         BNE   LR70                                                             
*                                                                               
LR100    CLC   YEAR,SIRKYEAR       TEST SAME YEAR                               
         BNE   LR70                                                             
*                                                                               
         CLI   0(R2),0             TEST END OF SCREEN                           
         BNE   LR110               NO                                           
         MVC   LASTKEY,KEY         YES - SAVE THE KEY                           
         MVC   CONHEAD(26),=C'Hit enter to continue list'                       
         B     LRX                                                              
*                                                                               
LR110    ZIC   R0,0(R2)            BUMP TO LIST LINE                            
         AR    R2,R0                                                            
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',SIRKMS),8(R2),39(R2) MARKET/STATION           
         CLI   39(R2),X'F0'        CABLE HEADEND?                               
         BL    *+8                                                              
         MVI   43(R2),C'/'         YES                                          
         MVC   SAVEKEY(13),KEY     SAVE NSID KEY                                
*                                                                               
         CLI   SVUSEREP,C'Y'       TEST READ REP MARKETS                        
         BNE   LR115               NO                                           
*                                                                               
         GOTO1 SWITCH,DMCB,=C'REP',0                                            
         CLI   DMCB+4,2            TEST REP SYSTEM IS STARTED                   
         BNE   *+12                YES                                          
         MVI   ERROR,REPISOFF                                                   
         B     TRAPERR                                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         LA    R5,KEY              READ MARKET KEY                              
         USING RMKTRECD,R5                                                      
         XC    KEY,KEY                                                          
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,8(R2)                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'REPDIR',KEY,KEY,0                 
         CLI   8(R1),0             TEST VALID MARKET                            
         BE    *+14                YES                                          
         MVC   13(24,R2),=CL24'*** UNKNOWN ***'                                 
         B     LR112                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'REPFIL',KEY+28,          +        
               AIO3,DMWORK                                                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3             DISPLAY MARKET NAME                          
         MVC   13(L'RMKTNAME,R2),RMKTNAME                                       
         DROP  R5                                                               
*                                                                               
LR112    GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    LR125               YES                                          
         DC    H'0'                                                             
*                                                                               
LR115    XC    KEY,KEY                                                          
         MVI   KEY,C' '            BUILD MARKET RECORD KEY                      
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVI   KEY+1,C'T'          MEDIA 'T'                                    
         MVC   KEY+2(4),8(R2)      MARKET NUMBER                                
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO3                                                          
         USING MKTRECD,R5                                                       
         CLC   KEY(8),0(R5)        DO WE NEED TO READ THE RECORD                
         BE    LR120               NO, WE HAVE ALREADY READ IT                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO3                 
LR120    LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   13(24,R2),0(R1)     DISPLAY MARKET NAME                          
*                                                                               
LR125    XC    KEY,KEY             RESTORE NSID KEY                             
         MVC   SIRKEY,SAVEKEY                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                RESTORE DATAMGR SEQUENCE                     
         CLC   SIRKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         MVC   49(1,R2),SIRKDPT    DISPLAY DAYPART                              
*                                                                               
         L     R5,AIO2             SCHEME RECORD                                
         MVI   ELCODE,EPNCODEQ     LOOK AT PERIOD NAMES ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,2(R5)            GO PAST OVERHEAD                             
LR130    MVC   BYTE,0(R5)                                                       
         OI    BYTE,SIRKBUYQ                                                    
         CLC   BYTE,SIRKMON        TEST SAME PERIOD                             
         BE    *+12                                                             
         LA    R5,5(R5)            NEXT PERIOD                                  
         B     LR130                                                            
         MVC   53(4,R2),1(R5)      PERIOD NAME                                  
*                                                                               
         MVC   0(18,R3),KEY        PUT KEY IN TABLE                             
         LA    R3,19(R3)           INCREMENT TABLE POINTER                      
*                                                                               
         OI    6(R2),FOUTTRN       XMIT LIST LINE                               
         ZIC   R0,0(R2)            BUMP TO NEXT SELECT                          
         AR    R2,R0                                                            
         B     LR70                GO FOR NEXT RECORD                           
*                                                                               
LRX      OI    SRLSELH+6,FOUTCUR   POSITION CURSOR                              
         OI    CONSERVH+6,FOUTTRN+FOUTMOD   SET MODIFIED AND TRANSMIT           
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLI   GCMODE,C'S'         TEST GENCON IS SLAVED                        
         BNE   LRX1                                                             
         GOTO1 SAVEUWKA            YES - MUST SAVE SYSD                         
*                                                                               
LRX1     XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* CONVERT DEMO CODES/VALUES (UPDATES R4)                                        
CNVRT    DS    0H                                                               
*                                                                               
         NMOD1 0,**CNVRT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         XC    BLOCK(256),BLOCK                                                 
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
         USING EDOELEM,R5                                                       
         LA    R3,DEMWRK                                                        
         XC    0(3,R3),0(R3)       EDODEMO=2 BYTES - BUT                        
         MVC   1(2,R3),EDODEMO     DEMOCON NEEDS 3                              
         GOTO1 DEMOCON,DMCB,(0,(R3)),(2,(R4)),(C'S',BLOCK)   7 CHAR             
*                                                                               
CNV10    CLI   0(R4),C' '          LOOK FOR END OF DEMNAME                      
         BNH   *+12                                                             
         LA    R4,1(R4)                                                         
         B     CNV10                                                            
*                                                                               
         MVI   0(R4),C'='          GOT IT - MOVE IN '='                         
         LA    R4,1(R4)                                                         
         EDIT  (2,EDOVALUE),(6,(R4)),1,ALIGN=LEFT                               
         DROP  R5                                                               
*                                                                               
CNV20    CLI   0(R4),C' '          LOOK FOR END OF VALUE                        
         BNH   *+12                                                             
         LA    R4,1(R4)                                                         
         B     CNV20                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         XIT1  REGS=(R4)           R4-NEXT POSSIBLE NAME=VALUE AREA             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* PREPARE FOR SELECT FROM NSID SCREEN                                           
*                                                                               
SELECT   DS    0H                                                               
*                                                                               
         NMOD1 0,**SEL***                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLC   SVDETADR,DMWORK+4   TEST WE MUST READ THE DETAIL RECORD          
         BE    SELECT5             NO                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),SYSFIL,SVDETADR,        +        
               AIO2,DMWORK                                                      
         CLI   8(R1),0             READ DETAIL RECORD INTO IO2                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SELECT5  L     R2,ASELFLDH         A(SELECT FIELD)                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO DAY                                  
         IC    R0,0(R2)                                                         
         AR    R2,R0               BUMP TO TIME                                 
         IC    R0,0(R2)                                                         
         AR    R2,R0               BUMP TO PRGTYPE                              
         MVI   SVPRGTYP,0                                                       
         CLI   5(R2),0             TEST PRGTYPE GIVEN                           
         BE    *+10                NO                                           
         MVC   SVPRGTYP,8(R2)      SAVE PRGTYPE                                 
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO COST                                 
         XC    SVCOST1,SVCOST1                                                  
         MVC   SVCOST1L,5(R2)                                                   
         CLI   SVCOST1L,0          TEST ANY COST GIVEN                          
         BE    SELECT10            NO                                           
*                                                                               
         ZIC   R1,SVCOST1L                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCOST1(0),8(R2)    SAVE COST                                    
*                                                                               
SELECT10 ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO UPGRADE                              
         XC    SVUPG,SVUPG                                                      
         MVC   SVUPGL,5(R2)                                                     
         CLI   SVUPGL,0            TEST ANY UPGRADE GIVEN                       
         BE    SELECT20            NO                                           
*                                                                               
         ZIC   R1,SVUPGL                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVUPG(0),8(R2)      SAVE UPGRADE                                 
*                                                                               
SELECT20 XC    DMCB(24),DMCB       SAVE TWA                                     
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,4            PAGE NUMBER                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',,(RA)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SAVEUWKA            SAVE SYSD                                    
         LA    RE,SYSSPARE+1024    CLEAR APPLICATION STORAGE                    
         LH    RF,=Y(SYSDEND-SYSSPARE-1024)                                     
         XCEF                                                                   
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* CLEAR SCREEN                                                                  
*                                                                               
CLEAR    DS    0H                                                               
*                                                                               
         NMOD1 0,**CLEAR*                                                       
*                                                                               
         LA    R2,SIRSELH          FIRST FIELD ON SCREEN                        
CLEAR10  NI    1(R2),X'FF'-FATBPROT UNPROTECT SELECT FIELD                      
         MVI   8(R2),0             CLEAR SELECT FIELD                           
         MVI   4(R2),0             RESET INPUT INDICATORS                       
         MVI   5(R2),0             CLEAR INPUT LENGTH                           
         OI    6(R2),FOUTTRN       XMIT                                         
*                                                                               
         ZIC   R0,0(R2)            BUMP TO FIRST DATA FIELD                     
         AR    R2,R0                                                            
         LA    RF,5                NO. OF DATA FIELDS PER ROW                   
*                                                                               
CLEAR20  ZIC   R1,0(R2)            R1 HAS LENGTH OF FIELD                       
         SH    R1,=H'9'            SUBTRACT HEADER LENGTH +1                    
         TM    1(R2),FATBXHDR      TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
*                                                                               
         EX    R1,*+8              PAD WITH BLANKS                              
         B     *+10                                                             
         OC    8(0,R2),=80C' '                                                  
         EX    R1,*+8              TEST FIELD EMPTY                             
         B     *+10                                                             
         CLC   8(0,R2),=80C' '                                                  
         BE    CLEAR30             YES                                          
         EX    R1,*+8              NO - CLEAR FIELD                             
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),FOUTTRN       XMIT                                         
*                                                                               
CLEAR30  TM    1(R2),FATBPROT      FIELD PROTECTED?                             
         BZ    *+12                NO                                           
         NI    1(R2),X'FF'-FATBPROT YES - UNPROTECT IT                          
         OI    6(R2),FOUTTRN       XMIT                                         
         MVI   4(R2),0             RESET ALL INPUT INDICATORS                   
         MVI   5(R2),0             CLEAR INPUT LENGTH                           
         ZIC   R0,0(R2)            BUMP                                         
         AR    R2,R0                                                            
         BCT   RF,CLEAR20          EACH DATA FIELD IN THE ROW                   
*                                                                               
         LA    RF,SIRPFH                                                        
         CR    R2,RF               TEST END OF SCREEN                           
         BNE   CLEAR10             NO - DO NEXT ROW                             
         XIT1                      YES - SO LONG                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* FUDGE INPUT LENGTH ON TWA FIELD                                               
*                                                                               
FUDGEINP DS    0H                                                               
*                                                                               
         NMOD1 0,FUDGEINP                                                       
*                                                                               
         ZIC   RF,0(R2)            FIELD LENGTH                                 
         SH    RF,=H'16'           MINUS (HEADER + EXTENSION)                   
         STC   RF,5(R2)            ASSUME INPUT LENGTH IS MAXIMUM               
*                                                                               
         ZIC   RF,0(R2)            FIELD LENGTH                                 
         AR    RF,R2               BUMP TO NEXT FIELD                           
         SH    RF,=H'9'            BACK UP JUST BEFORE EXTENSION                
         LA    R0,8(R2)            A(BEGINNING OF FIELD)                        
*                                                                               
FUDGEIN5 CLI   0(RF),C' '          TEST THERE'S A CHARACTER THERE               
         BH    FUDGEINX            YES - LEAVE                                  
         MVI   0(RF),0             REPLACE WHATEVER'S THERE WITH NULL           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)            DECREMENT INPUT LENGTH                       
         BCTR  RF,0                BACK UP ONE CHARACTER                        
*                                                                               
         CR    RF,R0               TEST WE'VE BACKED UP PAST THE FIELD          
         BNL   FUDGEIN5            NO                                           
         DC    H'0'                                                             
*                                                                               
FUDGEINX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMAED                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMBED                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
* WARNING -- IF THIS STORAGE EXCEEDS 1024 BYTES, THEN START OF                  
*            LOADED PHASE STORAGE MUST BE BUMPED UP ACCORDINGLY                 
*                                                                               
SAVERD   DS    A                                                                
DSKADD   DS    16A                 DETAIL RECS DSKADDRS (15 AND SPARE)          
ASELFLDH DS    A                   A(THIS SELECT FIELD)                         
LISTLEN  DS    H                   TOTAL NO. OF DAY/TIMES IN LIST               
LISTKEYS DS    17XL19              KEYS AND SELECT FLAGS OF NSID RECS           
LASTKEY  DS    XL13                LISTED KEY ON NSID LIST SCREEN               
LISTKEYK DS    XL13                KEY ON NSID LIST SCREEN                      
DSKADDX  DS    X                   DISP TO CURRENT DETAIL DISK ADDRESS          
SQNMDISP DS    X                   DISP INTO SEQUENCE NO. ARRAY                 
STARTSQN DS    X                   INDEX TO SCREEN'S 1ST SEQUENCE NO.           
SEQNUM   DS    X                   SEQUENCE NUMBER                              
PERNUM   DS    X                   PERIOD                                       
YEAR     DS    X                   YEAR                                         
CURYEAR  DS    X                   CURRENT YEAR                                 
DAYPART  DS    C                   DAYPART CODE                                 
COST     DS    XL4                 COST                                         
PROGTYP  DS    C                   PROGRAM CODE (IN FILTER)                     
PROGTYPE DS    C                   PROGRAM CODE (IN RECORD)                     
PRGTYPSV DS    CL170               PROGTYP SAVE (CODE + NAME)                   
SQNMS    DS    XL255               INDEXING SEQUENCE NOS.                       
DAYTIME  DS    0XL5                                                             
DAY      DS    X                   DAY                                          
TIME     DS    XL4                 TIME                                         
DEMWRK   DS    CL40                WORK AREA FOR DEMOVAL                        
OLDKEY   DS    XL13                ALWAYS HAS PREVIOUS KEY                      
SAVEKEY  DS    XL18                DELETED KEY                                  
INDEX    DS    X                   FOR COSTS                                    
NEWKEY   DS    C                   'Y' IF KEY HAS CHANGED                       
RESTART  DS    C                   'Y' IF LIST IS FROM BEGINNING                
MODSCRN  DS    C                   'Y' IF SCREEN WAS CHANGED                    
ADDDET   DS    C                   'Y' IF ADDREC OF DETAIL, OW PUTREC           
ADDNSID  DS    C                   'Y' IF ADDREC OF NSID, OW PUTREC             
FROMADD  DS    C                   'Y' IF DID ACTION ADD LAST TIME              
NOINVFLG DS    C                   'Y' IF NO INVENTORY IN NSID RECORD           
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDPERVALD                                                      
         DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPDEMUPD                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
RMKTRECD DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
         PRINT ON                                                               
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPSFM0E   08/11/11'                                      
         END                                                                    
