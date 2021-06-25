*          DATA SET GEFIL03S   AT LEVEL 031 AS OF 05/22/00                      
*PHASE T00AB3A                                                                  
*&&      SET   NOP=N                                                            
GEFIL03  TITLE 'NEW FILE CONTROLLER LIST OBJECT / ROUTINES'                     
*                                                                               
* 027 NRAK 23JUN99 CLEAR NEW (TSAR) LINE IF DLDVAL SETS LSLNIDEL                
* 026 TSMY 22MAR99 DON'T COPY ACTION WORD TO SUBACT IF NO SUBACT FIELD,         
*                  ALLOW FOR ERROR ON SCREEN FIRST AND DISLINE                  
*                                                                               
GEFIL03  START                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                               
* NTRY: P1 = OBJECT CODE                                              *         
*       P2 = EQUATED VERB                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
LIST     NMOD1 LCWORKL,**GF03**,RR=RE,CLEAR=YES                                 
         USING LCWORKD,RC                                                       
         USING TWAD,RA                                                          
         ST    RE,LCRELO                                                        
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
GSFRR    USING FRRELD,GSFRREL                                                   
PSFRR    USING FRRELD,PSFRREL                                                   
GSFRA    USING FRAELD,GSFRAEL                                                   
GSFRP    USING FRPELD,GSFRPEL                                                   
PSFRA    USING FRAELD,PSFRAEL                                                   
P        USING SSAVD,PSSAV                                                      
*                                                                               
         LA    RE,GENLST           SET UP ROUTINE ADDRESSES                     
         LA    R2,LCROUTS                                                       
         XR    RF,RF                                                            
         LA    R0,LCROUTSN                                                      
         ST    RE,0(R2)                                                         
         STC   RF,0(R2)                                                         
         LA    R2,L'LCROUTS(R2)                                                 
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         MVC   LCAREC,AIOREC                                                    
         MVC   LCEQUREC,=AL4(XIO11)                                             
         TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST?                            
         BZ    *+16                NO                                           
         MVC   LCAREC,AIO4                                                      
         MVC   LCEQUREC,=AL4(XIO4)                                              
*                                                                               
         CLM   R1,7,=XL3'FFFFFF'   SPECIAL EXTERNAL ROUTINE CALL                
         BNE   LISTNTR                                                          
         STCM  R1,8,LCBYTE                                                      
         ICM   RF,8,LCBYTE                                                      
         XC    LCBYTE,LCBYTE                                                    
         BAS   RE,GENLST                                                        
         B     EXIT                                                             
*                                                                               
LISTNTR  ST    R1,LCAR1                                                         
         MVC   RTPARMS,0(R1)                                                    
*                                                                               
         LA    R1,OBJTAB                                                        
         USING OBJTABD,R1                                                       
OBJ02    CLI   OBJTABD,EOT                                                      
         BE    EXITOK                                                           
         CLC   OBJVERB,RTPARMS1+3                                               
         BE    *+12                                                             
         LA    R1,OBJTABL(R1)                                                   
         B     OBJ02                                                            
         ICM   R7,15,OBJADR                                                     
         A     R7,LCRELO                                                        
         BR    R7                                                               
         DROP  R1                                                               
*                                                                               
OBJTAB   DS    0X                                                               
         DC    AL1(OLIST,0,0,0),AL4(LISTOBJ)                                    
         DC    AL1(OSUBACT,0,0,0),AL4(SUB)                                      
         DC    AL1(OSUBH,0,0,0),AL4(SHAC)                                       
         DC    AL1(OSCROLL,0,0,0),AL4(SCROLL)                                   
OBJTABX  DC    AL1(EOT)                                                         
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ENTRY POINT FOR INTERNAL LIST OBJECT CALLS                          *         
***********************************************************************         
         SPACE 1                                                                
GENLST   NTR1  ,                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         L     R7,GENLSTR(RF)                                                   
         A     R7,LCRELO                                                        
         BR    R7                                                               
*                                                                               
GENLSTR  DS    0A                                                               
         DC    A(LISTNTR)                                                       
         DC    A(GETREC)                                                        
         DC    A(CONT)                                                          
         DC    A(LSTMSG)                                                        
         DC    A(SCRDOWN)                                                       
         DC    A(SCRUP)                                                         
         DC    A(SCRHORZ)                                                       
         DC    A(CURLIN)                                                        
         DC    A(SETPAG)                                                        
         DC    A(DISPAG)                                                        
         DC    A(VALPAG)                                                        
         DC    A(VALNPAG)                                                       
         DC    A(DISLINE)                                                       
         DC    A(DISTOT)                                                        
         DC    A(VALLINE)                                                       
         DC    A(VALNLIN)                                                       
         DC    A(FNDMIF)                                                        
         DC    A(TSTMIF)                                                        
         DC    A(ADDMIF)                                                        
         DC    A(VALFLD)                                                        
         DC    A(REMLINE)                                                       
         DC    A(CLRLINE)                                                       
         DC    A(TSTLINE)                                                       
         DC    A(SCRHAMT)                                                       
         DC    A(SCRVAMT)                                                       
         DC    A(GETITEM)                                                       
         DC    A(NEWITEM)                                                       
         DC    A(NXTSCRN)                                                       
         DC    A(PUTITEM)                                                       
         DC    A(SAVITEM)                                                       
         DC    A(SHUFFLE)                                                       
         DC    A(INISCR)                                                        
         DC    A(BLDTOP)                                                        
         DC    A(BLDBOT)                                                        
         DC    A(BLDLST)                                                        
         DC    A(BLDSCR)                                                        
         DC    A(BLDFIX)                                                        
         DC    A(BLDVAR)                                                        
         DC    A(BLDREV)                                                        
         DC    A(BLDSPC)                                                        
         DC    A(SPCOUT)                                                        
         DC    A(BLDHED)                                                        
         DC    A(BLDLIN)                                                        
         DC    A(BLDMUL)                                                        
         DC    A(SAVBOT)                                                        
         DC    A(RESBOT)                                                        
         DC    A(BLDROW)                                                        
         DC    A(ADDLIN)                                                        
         DC    A(DELLIN)                                                        
         DC    A(BORDER)                                                        
         DC    A(GETNXT)                                                        
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR LITERALS                                                  *         
***********************************************************************         
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   GEFIL03+X'1000'                                                  
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
LISTOBJ  LA    R1,LISTTAB          KNOWN VERBS FOR LIST OBJECT                  
         USING OBJTABD,R1                                                       
*                                                                               
LIST02   CLI   OBJVERB,EOT                                                      
         BE    LIST08                                                           
         CLC   OBJVERB,LCVERB                                                   
         BNE   *+14                MATCHED                                      
         CLC   OBJSVB,LCSVB                                                     
         BE    *+12                                                             
         LA    R1,OBJTABL(R1)                                                   
         B     LIST02              BUMP & LOOP                                  
*                                                                               
LIST04   TM    OBJIND1,OBJPRIV     PRIVATE?                                     
         BZ    LIST06                                                           
         L     RE,4(RD)            MAKE SURE INVOKED AT THIS LEVEL              
         L     RF,4(RE)                                                         
         CLC   16(4,RE),16(RF)                                                  
         BE    LIST06                                                           
         DC    H'0'                                                             
*                                                                               
LIST06   ICM   R6,15,OBJADR                                                     
         A     R6,LCRELO                                                        
         BR    R6                                                               
         DROP  R1                                                               
*                                                                               
LIST08   B     EXITH               NOT KNOWN AT THIS LEVEL                      
*                                                                               
LISTTAB  DC    AL1(LPRC,0,0,0),AL4(PROC)                                        
         DC    AL1(LPRC,0,0,ACT1ST),AL4(PROC)                                   
         DC    AL1(LPRC,0,0,ACTNTR),AL4(PROCNTR)                                
         DC    AL1(LPRC,0,0,ACTXIT),AL4(PROCXIT)                                
*                                                                               
         DC    AL1(LINIT,0,0,0),AL4(INIT)                                       
         DC    AL1(LSCRFRST,0,0,0),AL4(SCRFRST)                                 
         DC    AL1(LSCRLAST,0,0,0),AL4(SCRLAST)                                 
         DC    AL1(LLSTFRST,0,0,0),AL4(LSTFRST)                                 
         DC    AL1(LLSTLAST,0,0,0),AL4(LSTLAST)                                 
         DC    AL1(LGETFRST,0,0,0),AL4(GETFRST)                                 
         DC    AL1(LGETNEXT,0,0,0),AL4(GETNEXT)                                 
         DC    AL1(LTSARADD,0,0,0),AL4(TSARADD)                                 
         DC    AL1(LTSARDIR,0,0,0),AL4(TSARDIR)                                 
         DC    AL1(LTSARFIL,0,0,0),AL4(TSARFIL)                                 
         DC    AL1(LTSARTSA,0,0,0),AL4(TSARTSA)                                 
         DC    AL1(LPFKPOS,0,0,0),AL4(PFKPOS)                                   
         DC    AL1(LDEFCLM,0,0,0),AL4(DEFCLM)                                   
         DC    AL1(LTSARDEL,0,0,0),AL4(TSARDEL)                                 
         DC    AL1(LMNTUPD,0,0,0),AL4(LSTUPD)                                   
         DC    AL1(LLSTUPD,0,0,0),AL4(LSTUPD)                                   
         DC    AL1(LUPDFRST,0,0,0),AL4(UPDFRST)                                 
         DC    AL1(LUPDLAST,0,0,0),AL4(UPDLAST)                                 
         DC    AL1(LUPDDIR,0,0,0),AL4(UPDDIR)                                   
         DC    AL1(LUPDREC,0,0,0),AL4(UPDREC)                                   
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS LIST - RETURN FROM XITSES                              *  1 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
N        USING SSAVD,NSSAV                                                      
PROCXIT  CLI   NSREC,O#FLTR        JUST BACK FROM FILTERING?                    
         BNE   PRCX02              NO                                           
         GOTOX AGEN,RTPARM,OFILT,FDIS,LSINIKEY                                  
         OI    LSSCIND1,LSSCIFLT                                                
         B     PRCX08                                                           
*                                                                               
PRCX02   CLI   NSREC,O#DIS         JUST BACK FROM SETTING COLUMNS?              
         BNE   PRCX04              NO                                           
         OI    LSSCIND1,LSSCICLM                                                
         B     PRCX08                                                           
*                                                                               
PRCX04   TM    LSSTAT1,LSSINIT     LIST STILL OK TO PROCESS?                    
         BO    *+18                NO                                           
         XC    LCSVB,LCSVB                                                      
         L     R1,LCAR1                                                         
         B     GENLST                                                           
*                                                                               
         TM    N.SNINDS1,SNIUSECR  JUST BACK FROM A SELECT ACTION?              
         BZ    PRCXX               NO                                           
         MVC   LCBYTE,LSSCIND1                                                  
         NI    LCBYTE,LSSCIINP+LSSCIMEP+LSSCIMEL                                
*                                                                               
         GOTOX ALST,RTPARM,OLIST,LSCRFRST                                       
         BL    PRCXX                                                            
*                                                                               
         OC    LSSCIND1,LCBYTE                                                  
         GOTOX ALST,RTPARM,OSUBACT,SADONE                                       
         GOTOX ADISLINE                                                         
*                                                                               
         CLI   GSFRP.FRPTYPE,FRPTQPL  USER PRESSED 'QUIT' PFKEY?                
         BNE   PRCX06               NO                                          
         LH    RF,LSCURLIN                                                      
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$ACTOK)                                           
         B     PRCXX                                                            
*                                                                               
PRCX06   LH    RE,LSLINE#                                                       
         LA    RE,1(RE)                                                         
         STH   RE,LSLINE#                                                       
         GOTOX AVALPAG                                                          
         LH    RF,LS1STINP                                                      
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$ACTOK)                                           
         B     PRCXX                                                            
*                                                                               
PRCX08   GOTOX ACONT               ROUTINE TO CONTROL THE LIST                  
         BL    PRCXX                                                            
*                                                                               
         TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST SETS OWN MESSAGE            
         BO    PRCXX                                                            
         GOTOX ALSTMSG             SET MESSAGE                                  
*                                                                               
PRCXX    GOTOX ALST,RTPARM,OLIST,LSCRLAST                                       
         BL    EXITL                                                            
         TM    LSSCIND2,LSSCIOK    LIST PROCESSED OK?                           
         BZ    EXITL               NO                                           
         OI    GCINDS1,GCIMLPRC    SET MAINT LIST PROCESSED FLAG                
         B     EXITOK                                                           
         DROP  N                                                                
         SPACE 2                                                                
***********************************************************************         
* PROCESS LIST - FIRST TIME AFTER NTRSES                         *  1 *         
* PROCESS LIST - NORMAL                                          ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
PROCNTR  DS    0H                  SAME CODE FOR JUST NTRSES'D                  
PROC     TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST?                            
         BZ    PROC02              NO                                           
         TM    LSSCIND2,LSSCIDIS   DISPLAY ONLY?                                
         BZ    PROC02              NO                                           
         TM    GCINDS1,GCIMLPRC    PROCESSED ALREADY?                           
         BZ    PROC02              NO                                           
         B     EXITOK                                                           
*                                                                               
PROC02   TM    BCINDS1,(BCINREC+BCINACT)  NEW RECORD ºº ACTION?                 
         BZ    *+8                 NO                                           
         NI    LSSTAT1,FF-LSSINIT  RE-INITIALISE LIST                           
*                                                                               
         TM    GCINDS2,GCINTRS     JUST NTRSES'D?                               
         BZ    *+8                 NO                                           
         NI    LSSTAT1,FF-LSSINIT  REINITIALISE THE LIST                        
*                                                                               
         TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST?                            
         BZ    PROC02A             NO                                           
         TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE ROWS?                       
         BZ    PROC02A             NO                                           
         CLC   GSRECKEY,GCLASKEY   CHANGE OF KEY?                               
         BE    *+8                 NO                                           
         NI    LSSTAT1,FF-LSSINIT  MUST REINITIALISE THE LIST                   
*                                                                               
PROC02A  TM    LSSTAT1,LSSINIT     LIST NEEDS INITIALISING?                     
         BO    PROC04              NO                                           
         GOTOX ALST,RTPARM,OLIST,LINIT                                          
*                                                                               
         TM    LSSTAT1,LSSENTK     'ENTER KEY' REQUESTED?                       
         BZ    PROC04              NO                                           
*                                                                               
         OC    GSDSPKEY,GSDSPKEY   'KEY' FIELD ON SCREEN?                       
         BZ    PROC03                                                           
         LH    RF,GSDSPKEY                                                      
         A     RF,ATWA                                                          
         USING FHD,RF                                                           
         CLI   FHIL,0              INPUT TO 'KEY' FIELD                         
         BNE   PROC04              YES - CONTINUE AS NORMAL                     
         DROP  RF                                                               
*                                                                               
PROC03   MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$ENFLT)                                           
         LH    RF,LS1STKEY         DISPLACEMENT TO FIRST KEY FIELD              
         A     RF,ATWA                                                          
         ST    RF,FVADDR           SET CURSOR ON FIRST KEY FIELD                
         B     PRCX                                                             
*                                                                               
PROC04   GOTOX ALST,RTPARM,OLIST,LSCRFRST                                       
         BL    PRCX                ERROR IN FIRST FOR SCREEN                    
                                                                                
         TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST?                            
         BZ    PROC06              NO                                           
*                                                                               
         TM    BCINDS1,(BCINACT+BCIANYPF)  NEW ACTION OR PFK PRESS?             
         BZ    *+8                 NO                                           
         OI    LSSCIND1,LSSCIINP   YES - SUPPRESS AUTOMATIC SCROLL              
*                                                                               
         CLC   GSRECKEY,GCLASKEY   REBUILD LIST ON CHANGE OF KEY                
         BE    *+8                                                              
         NI    LSLTIND1,FF-(LSLTIBLD)                                           
*                                                                               
PROC06   GOTOX ACONT               ROUTINE TO CONTROL THE LIST                  
         BL    PRCX                                                             
*                                                                               
         TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST SETS OWN MESSAGE            
         BO    PRCX                                                             
         GOTOX ALSTMSG             SET MESSAGE                                  
*                                                                               
PRCX     GOTOX ALST,RTPARM,OLIST,LSCRLAST                                       
         BL    EXITL                                                            
         TM    LSSCIND2,LSSCIOK    LIST PROCESSED OK?                           
         BZ    EXITL               NO                                           
         OI    GCINDS1,GCIMLPRC    SET MAINT LIST PROCESSED FLAG                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
*INITIALISE LIST                                                 *  2 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
INIT     MVI   LSSTAT1,LSSINIT+LSSENTK   RESET ALL VALUES                       
         MVI   LSSTAT2,0                                                        
         MVI   LSSTAT3,0                                                        
         XC    LSLTIND1,LSLTIND1                                                
         XC    LSDSPROW,LSDSPROW                                                
         XC    LSINIKEY,LSINIKEY   ?? AATK 5 FEB 97                             
*                                                                               
         CLI   LSSSEOP,0           SELECT TO END OF PAGE SET?                   
         BNE   *+8                 YES                                          
         MVI   LSSSEOP,C'+'        SET DEFAULT                                  
         CLI   LSSSEOL,0           SELECT TO END OF LIST SET?                   
         BNE   *+8                 YES                                          
         MVI   LSSSEOL,C'&&'       SET DEFAULT                                  
*                                                                               
         TM    GSINDSL1,GSIMLST    MAINTENANCE LIST?                            
         BZ    *+12                NO                                           
         OI    LSSTAT1,LSSMAIN                                                  
         NI    LSSTAT1,FF-LSSENTK                                               
*                                                                               
         TM    GCINDS2,GCINTRS     JUST NTRSES'D?                               
         BZ    INIT04              NO                                           
         TM    P.SNINDS1,SNISEL    'SELECT' TYPE LIST?                          
         BZ    *+8                 NO                                           
         OI    LSSTAT1,LSSSEL      SET 'SELECT' TYPE LIST                       
         XR    RF,RF                                                            
         ICM   RF,3,P.SSELDSP      TEST SELECT FIELD ON THE SCREEN              
         BZ    *+12                                                             
         LA    RF,TWAD(RF)                                                      
         OI    FHIID(RF),FHIIVA    FORCE USER TO INPUT FIELD                    
*                                                                               
         TM    P.SNINDS1,SNIBLDKY  APPLICATION CAN BUILD OWN KEY?               
         BZ    *+8                                                              
         NI    LSSTAT1,FF-LSSENTK  YES - DON'T NEED TO ENTER KEY                
*                                                                               
         TM    P.SNINDS1,SNIPARMS  CALLER PASSED PARAMETERS?                    
         BZ    *+8                                                              
         NI    LSSTAT1,FF-LSSENTK  YES - DON'T NEED TO ENTER KEY                
*                                                                               
INIT04   MVC   LSNUMFTL,=AL2(1)    DEFAULT NO. OF FOOTLINES                     
         MVC   LSNUMHED,=AL2(2)    DEFAULT NO. OF HEADLINES                     
         MVI   LSSUBLEN,4          DEFAULT SUB-ACTION LENGTH                    
         XC    LSTOTTOP,LSTOTTOP                                                
         XC    LSTOTBOT,LSTOTBOT                                                
*                                                                               
         TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST?                            
         BZ    INIT06              NO                                           
         XC    LSNUMFTL,LSNUMFTL   YES - DIFFERENT DEFAULTS                     
         MVI   LSSUBLEN,0                                                       
         OI    LSSTAT1,LSSBALL     BUILD ENTIRE LIST                            
         OI    LSSTAT2,LSSADD      ALLOW ADDING TO LIST                         
*                                                                               
INIT06   MVC   LSCOLROW,=AL2(COLS#Q)                                            
         MVC   LSCOLLIN,=AL2(COLS#Q)                                            
         MVC   LSLINROW,=AL2(1)                                                 
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',OLIST),LINIT,0,0                          
         BL    EXITL               ERROR ALLOWING APP. TO OVERRIDE              
*                                                                               
         TM    LSSTAT3,LS3RVAR     VARIABLE ROWS REQUIRE ENTIRE LIST            
         BZ    *+8                                                              
         OI    LSSTAT1,LSSBALL                                                  
*                                                                               
         GOTOX ALST,RTPARM,OLIST,LDEFCLM                                        
         BL    EXITL               ERROR SETTING DEFAULT COLUMNS                
*                                                                               
         CLI   CSREC,O#FLTR        CLEAR FILTER AREA                            
         BE    INIT08              (UNLESS RECORD IS 'FILTER')                  
         LH    RE,=Y(FLTELSV-TWAD)                                              
         A     RE,ATWA                                                          
         LA    RF,FLTELSVL                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
INIT08   MVC   LCAREC,AIOREC       NORMAL LIST USES AIOREC TO BUILD IN          
         MVC   LCEQUREC,=AL4(XIO11)                                             
         TM    LSSTAT1,LSSMAIN                                                  
         BZ    *+16                                                             
         MVC   LCAREC,AIO4         MAINTENANCE LIST USES AIO4                   
         MVC   LCEQUREC,=AL4(XIO4)                                              
*                                                                               
INITX    GOTOX AINISCR             INITIALISE SCREEN                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
*FIRST FOR SCREEN                                                *  3 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
SCRFRST  MVI   LSSCIND1,0          RESET SCREEN INDICATORS                      
*        MVI   LSSCIND2,0                                                       
         NI    LSSCIND2,LSSCIDIS   PRESERVE DISPLAY STATUS                      
*                                                                               
         TM    LSSTAT1,LSSSEL      LISTING TO SELECT?                           
         BZ    SFRST02             NO                                           
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,P.SSELDSP      SELECT FIELD ON SCREEN?                      
         BZ    SFRST02             NO                                           
*                                                                               
         LA    R3,TWAD(R3)         R3=A(SELECT FIELD)                           
         TM    FHIID(R3),FHIIVA    FIELD HAS BEEN INPUT?                        
         BO    SFRST02             NO                                           
*                                                                               
         OI    P.SXINDS1,SXISELIN  SET 'USER INPUT SELECT FIELD' FLAG           
         GOTOX AGEN,RTPARM,OSES,SXIT                                            
         DC    H'0'                                                             
*                                                                               
SFRST02  GOTOX ALST,RTPARM,OSCROLL,SCVAL                                        
         BL    EXITL               ERROR ON VALIDATE OF SCROLL FIELD            
*                                                                               
         TM    GCINDS2,GCINTRS     DID WE JUST NTRSES?                          
         BZ    SFRST08             NO                                           
         TM    P.SNINDS1,SNIPARMS  PASSING PARAMETERS?                          
         BO    SFRST08             NO                                           
         TM    P.SNINDS1,SNIBLDKY  BUILDING NEW KEY?                            
         BZ    SFRST08             NO                                           
         XC    LSINIKEY,LSINIKEY   RESET INITIAL KEY                            
*                                                                               
         GOTOX AGEN,RTPARM,OFILT,FBLDKEY,LSINIKEY                               
         BL    EXITL               ERROR BUILDING INITIAL KEY                   
*                                                                               
         GOTOX AGEN,RTPARM,OKEY,KFDIS,LSINIKEY                                  
         B     SFRST10                                                          
*                                                                               
SFRST08  TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST?                            
         BO    SFRST10             YES                                          
*                                                                               
         XC    LSINIKEY,LSINIKEY                                                
         GOTOX AGEN,RTPARM,OKEY,KFLD                                            
*                                                                               
         GOTOX AGEN,RTPARM,OFILT,FVAL,LSINIKEY                                  
         BL    EXIT                                                             
*                                                                               
         GOTOX AOLY,RTPARM,OLIST,LKEYCLM,LSINIKEY                               
*                                                                               
         TM    LSSCIND1,LSSCICLM   KEY COLUMNS CHANGED?                         
         BZ    SFRST10             NO                                           
*                                                                               
         GOTOX AINISCR             REBUILD SCREEN                               
*                                                                               
         XC    GSRECKEY,GSRECKEY                                                
         XC    GSRECSTA,GSRECSTA                                                
         XC    GSRECDA,GSRECDA                                                  
         LH    RF,GSDIRDSP                                                      
         A     RF,AXFILTAB                                                      
         USING NFITABD,RF                                                       
         LA    R2,LSINIKEY         INITIAL KEY BUILD AREA                       
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,NFIKEYL        LENGTH OF RECORD KEY                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GSRECKEY(0),0(R2)   MOVE IN KEY                                  
*                                                                               
         LA    R1,3(RE,R2)         2 IS FOR RECORD LENGTH                       
         ICM   RE,1,NFICTLL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GSRECSTA(0),0(R1)   MOVE IN STATUS                               
*                                                                               
         GOTOX AGEN,RTPARM,OKEY,KFDIS,LSINIKEY                                  
*                                                                               
SFRST10  OC    LSTOTTOP,LSTOTTOP   TOTAL LINE AT TOP?                           
         BZ    SFRSTX              NO                                           
         TM    LSLTIND1,LSLTISOL   STARTED LIST YET?                            
         BZ    SFRSTX              NO                                           
*                                                                               
         XR    R3,R3                                                            
         LH    R0,LSTOTTOP         NUMBER OF TOTAL LINES AT TOP                 
         LH    R2,LSTTPDSP                                                      
         A     R2,ATWA             R2=A(FIRST TOTAL LINE)                       
         USING FHD,R2                                                           
*                                                                               
SFRST12  GOTOX ADISTOT,RTPARM,FHD,((R3),DDISTOP)                                
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    R2,0(RF,R2)         NEXT FIELD ON SCREEN                         
         LA    R3,1(R3)            BUMP TOTAL LINE NUMBER                       
         BCT   R0,SFRST12          DO FOR ALL TOTAL LINES                       
         DROP  R2                                                               
*                                                                               
SFRSTX   GOTOX APRG,RTPARM,('GCBOVER',OLIST),LSCRFRST                           
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*LAST FOR SCREEN                                                 *  4 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
SCRLAST  OC    LSTOTBOT,LSTOTBOT   TOTAL LINES AT BOTTOM?                       
         BZ    SLAST04             NO                                           
         TM    LSLTIND1,LSLTISOL   LIST STARTED YET?                            
         BZ    SLAST04             NO                                           
*                                                                               
         GOTOX AGETITEM,LSPAG#X    GET LAST LIST ITEM PROCESSED                 
         XR    R3,R3                                                            
         LH    R0,LSTOTBOT         NUMBER OF TOTAL LINES AT BOTTOM              
         LH    R2,LSTBTDSP                                                      
         A     R2,ATWA             R2=A(FIRST BOTTOM TOTAL LINE)                
         USING FHD,R2                                                           
*                                                                               
SLAST02  GOTOX ADISTOT,RTPARM,FHD,((R3),DDISBOT)                                
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    R2,0(RF,R2)         NEXT FIELD ON SCREEN                         
         LA    R3,1(R3)            INCREMENT LINE NUMBER                        
         BCT   R0,SLAST02          ITERATE FOR ALL TOTAL LINES                  
         DROP  R2                                                               
*                                                                               
SLAST04  TM    LSSTAT2,LSSIUPD     OVERLAY WANTS TO DO UPDATES?                 
         BZ    SLAST06             NO                                           
         TM    LSSCIND2,LSSCITSC   TSAR RECORDS CHANGED?                        
         BZ    SLAST06             NO                                           
*                                                                               
         GOTOX AGENLST,RTPARM,OLIST,LLSTUPD                                     
         BL    EXITL                                                            
*                                                                               
SLAST06  GOTOX APRG,RTPARM,('GCBOVER',OLIST),LSCRLAST                           
         BL    EXITL                                                            
*                                                                               
         CLI   CSREC,O#RTYP        CONTROLLER SELECT RECORDS                    
         BE    SLAST08                                                          
         CLI   CSREC,O#ACT                                                      
         BE    SLAST08                                                          
         CLI   CSREC,O#SUBAC                                                    
         BNE   EXITOK                                                           
*                                                                               
         CLI   PSREC,O#SUBAC       NO AUTOSELECT IF ONE OF THESE                
         BE    EXITOK              AND WANTING SUB-ACTION HELP                  
         CLI   PSREC,O#RTYP                                                     
         BE    EXITOK                                                           
         CLI   PSREC,O#ACT                                                      
         BE    EXITOK                                                           
*                                                                               
SLAST08  TM    LSLTIND1,(LSLTISOL+LSLTIEOL)   END OF LIST FOUND                 
         BNO   EXITOK                                                           
         CLC   LSLST#1,LSLST#X     ONLY 1 ITEM IN LIST?                         
         BNE   EXITOK              NO                                           
*                                                                               
         STH   RF,LSCURLIN         SET CURRENT LINE TO BE FIRST LINE            
         MVC   LSCURLIN,LS1STLIN                                                
*                                                                               
         MVC   LSLINE#,LSLST#1                                                  
         GOTOX AGETITEM,LSLINE#    GET FIRST LIST RECORD                        
         OI    GCINDS2,GCILOAD     ENSURE CALLER'S OVERLAY IS RELOADED          
*                                                                               
         OI    LSSTAT1,LSSSEL      SET IT WAS SELECTED                          
         GOTOX ALST,RTPARM,OSUBH,SHSEL                                          
         DC    H'0'                NEVER COME BACK                              
         EJECT                                                                  
         SPACE 2                                                                
***********************************************************************         
*FIRST FOR BUILDING LIST                                         *  5 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
LSTFRST  MVI   LSLTIND1,LSLTIBLD   SET LIST BUILD BEGUN                         
         MVI   LSLTIND2,0                                                       
         XC    LSLST#1,LSLST#1     RESET FIRST LIST RECORD NUMBER               
         XC    LSLST#X,LSLST#X     RESET LAST LIST RECORD NUMBER                
         XC    LSSEQ#,LSSEQ#       RESET BUILD LIST SEQUENCE NUMBER             
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
*                                                                               
LFRST02  XC    TLKEY,TLKEY         DELETE ALL RECORDS AT THIS LEVEL             
         MVC   TLKSES,TWASESNL                                                  
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    LFRST04             END OF FILE                                  
         CLC   TLKSES,TWASESNL     STILL AT THIS LEVEL?                         
         BNE   LFRST04             NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         B     LFRST02                                                          
*                                                                               
LFRST04  GOTOX APRG,RTPARM,('GCBOVER',OLIST),LLSTFRST,LCRECKEY                  
         BL    EXITL                                                            
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
*LAST FOR BUILDING LIST                                          *  6 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
LSTLAST  GOTOX AOLY,RTPARM,OLIST,LLSTLAST                                       
         BL    EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
*GET FIRST RECORD FOR LIST PAGE                                  *  7 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
GETFRST  LA    R3,LGETFRST                                                      
         GOTOX AGETNXT                                                          
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*GET NEXT RECORD FOR LIST PAGE                                   *  8 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
GETNEXT  LA    R3,LGETNEXT                                                      
         GOTOX AGETNXT                                                          
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*ADD TSAR RECORD TO LIST                                         *  9 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
TSARADD  TM    LSSTAT2,LSTSFULL    TEST TSAR BUFFER FULL                        
         BO    EXITL                                                            
*                                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         MVC   TLKSES,TWASESNL                                                  
         TM    LSSTAT2,LSSNOSEQ    TEST DON'T PUT SEQ# ON KEY                   
         BO    *+10                                                             
         MVC   TLKSEQ,LSSEQ#       SET SEQUENCE NUMBER                          
         ICM   RF,3,LSSEQ#                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,3,LSSEQ#         INCREMENT SEQUENCE NUMBER                    
*                                                                               
         GOTOX ('TSARIO',AGROUTS),TSAADD                                        
         BNE   EXITL                                                            
*                                                                               
         TM    LSLTIND1,LSLTISOL   STARTED LIST BEFORE?                         
         BO    TADD02              YES                                          
*                                                                               
         OI    LSLTIND1,LSLTISOL   STARTED IT NOW.                              
         MVC   LSLST#1,TLNUM       FIRST IN LIST                                
         MVC   LSPAG#1,TLNUM       FIRST IN CURRENT PAGE                        
         MVC   LSLST#X,TLNUM       LAST IN LIST                                 
         XR    RF,RF                                                            
         ICM   RF,3,TLNUM                                                       
         AH    RF,LSLINPAG         NUMBER OF LIST ITEMS ON A PAGE               
         BCTR  RF,0                ZERO BASE IT                                 
         STCM  RF,3,BLLST#X        SET NEW LIST HIGH NUMBER                     
         B     TADDX                                                            
*                                                                               
TADD02   XR    RF,RF                                                            
         ICM   RF,3,LSLST#X        UPDATE LAST NUMBER IN LIST                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,LSLST#X                                                     
*                                                                               
TADDX    B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
*POSITIONAL PFKEY PRESSED                                        * 10 *         
*                                                                ******         
* NTRY: P3 = A(NEW RECORD/ACTION FOR PFKEY TABLE ENTRY)               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
PFKPOS   L     RF,RTPARMS3                                                      
         MVC   LCHALF,0(RF)        LCHALF=RECORD/ACTION                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,CSCURDSP       RF=CURSOR DATA SCREEN ADDRESS                
         GOTOX ACURLIN,RTPARM,(RF)                                              
         BNE   PFPOSL              NOT ONE OF LINES ON LIST                     
*                                                                               
         L     R3,4(R1)                                                         
         USING FHD,R3              R3=A(SUB-ACTION INPUT FIELD)                 
*                                                                               
         XR    R4,R4                                                            
         IC    R4,LCHALF           R4=RECORD REQUESTED                          
         XR    R5,R5                                                            
         IC    R5,LCHALF+1         R5=ACTION REQUESTED                          
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTTST,((R4),0)                                
         BNE   PFPOSL              INVALID RECORD                               
*                                                                               
         GOTOX (RF),(R1),OACT,ATST,((R5),(R4))                                  
         BNE   PFPOSL              INVALID ACTION                               
*                                                                               
         CLI   LSSUBLEN,0          EXIT IF NO SUBACT FIELD                      
         BE    EXITOK                                                           
         L     R4,12(R1)                                                        
         USING FRAELD,R4           R4=A(ACTION ELEMENT)                         
         MVC   LCWORK1,BCSPACES                                                 
         MVI   LCWORK1,GE#ESCL                                                  
         MVC   LCWORK1+1(L'FRADICT),FRADICT                                     
         MVC   LCWORK1+3(L'LSSUBLEN),LSSUBLEN                                   
         ICM   RF,15,=CL4'SU  '                                                 
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),LCWORK1                                      
*                                                                               
         MVC   LCBYTE,FHDA         COPY ACTION WORD TO SUB-ACT INPUT            
         XR    RE,RE                                                            
         IC    RE,LSSUBLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),LCWORK1                                                  
*                                                                               
         CLC   LCBYTE,LSSSEOP      MODIFIER?                                    
         BE    *+14                YES                                          
         CLC   LCBYTE,LSSSEOL                                                   
         BNE   *+14                NO                                           
         LA    RE,FHDA(RE)                                                      
         MVC   0(1,RE),LCBYTE                                                   
         STC   RE,FHIL                                                          
         OI    FHOI,FHOITR         SHOW FIELD                                   
         B     EXITOK                                                           
         DROP  R4,R3                                                            
*                                                                               
PFPOSL   OI    FVCURIND,FVCKEEP                                                 
         MVC   FVMSGNO,=AL2(GE$NOPOS)                                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
*SET UP TSAR RECORD FROM DIRECTORY                               * 11 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
TSARDIR  L     R2,ATLST            INITIALISE TSAR RECORD                       
         USING TLSTD,R2                                                         
         LA    RE,TLSTD                                                         
         LA    RF,L'TLST                                                        
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0               CLEAR BUFFER                                 
*                                                                               
         MVC   TLRLEN,=AL2(TLMINLNQ)  SET DEFAULT LENGTH                        
*                                                                               
         LH    RF,GSDIRDSP         SAVE DIRECTORY RECORD INFORMATION            
         A     RF,AXFILTAB                                                      
         USING NFITABD,RF                                                       
         XR    RE,RE                                                            
         LA    R3,LCRECKEY         WHERE DIRECTORY RECORD IS                    
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TLRKEY(0),0(R3)     MOVE IN KEY                                  
*                                                                               
         LA    R3,1(RE,R3)                                                      
         IC    RE,NFICTLL          LENGTH OF CONTROL AREA                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TLRSTA(0),0(R3)     MOVE IN STATUS                               
*                                                                               
         LA    R3,1(RE,R3)                                                      
         MVC   TLRDA,0(R3)         SAVE RECORD DISK ADDRESS                     
*                                                                               
         TM    NFIINDS2,NFIIID     LINKED D/A?                                  
         BO    TDIR02              YES                                          
*                                                                               
         TM    NFIINDS,NFIIVL      NO - MUST BE V/L IN THAT CASE                
         BO    *+6                                                              
         DC    H'0'                WHAT IS THIS FILE THEN?                      
*                                                                               
         MVC   TLRDA,BCEFFS        SET SPECIAL DISK ADDRESS                     
         DROP  RF                                                               
*                                                                               
TDIR02   OC    TLRDA,TLRDA         DO WE HAVE A DISK ADDRESS                    
         BNZ   *+8                                                              
         OI    TLRINFO1,TLR1XFIL   SET RECORD NOT ON FILE                       
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',OLIST),LTSARDIR,LCRECKEY,ATLST            
         BL    EXITL                                                            
*                                                                               
         MVC   GSRECMSK,BCEFFS     SET MASK TO X'FF'S                           
         GOTOX APRG,RTPARM,('GCBOVER',OKEY),KMASK,LCRECKEY,ATLST                
         MVC   TLRMSK,GSRECMSK                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
*SET UP TSAR RECORD FROM FILE RECORD                             * 12 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
TSARFIL  L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         GOTOX APRG,RTPARM,('GCBOVER',OLIST),LTSARFIL,LCAREC,ATLST              
         BL    EXITL                                                            
*                                                                               
         MVC   GSRECMSK,TLRMSK                                                  
         GOTOX APRG,RTPARM,('GCBOVER',ORECH),RMASK,LCAREC,ATLST                 
         MVC   TLRMSK,GSRECMSK                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
*SET UP DEFAULT COLUMN LIST                                      * 13 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
DEFCLM   LA    RE,LSFIXNUM         CLEAR COLUMN INFORMATION                     
         LA    RF,LSLINS                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTOX AOLY,RTPARM,OLIST,LDEFCLM  OLD WAY OF DOING THINGS               
         OC    LSFIXNUM,LSFIXNUM                                                
         BNZ   DCLMX                                                            
         OC    LSVARNUM,LSVARNUM                                                
         BNZ   DCLMX                                                            
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         LA    R3,LSFIXCLM                FIXED COLUMNS                         
F        USING DCTABD,R3                                                        
         LA    R4,LSVARCLM                VARIABLE COLUMNS                      
V        USING DCTABD,R4                                                        
         LA    R5,LSVALCLM                VALID FOR INCLUSION COLUMNS           
O        USING DCTABD,R5                                                        
*                                                                               
K        USING FCRRECD,IOKEY                                                    
         LA    R2,CLMTAB                                                        
DCLM02   CLI   0(R2),EOT           ANY COMBINATIONS LEFT TO TRY?                
         BE    DCLM20              NO - NEED TO SET A DEFAULT                   
*                                                                               
         XC    K.FCRKEY,K.FCRKEY                                                
         MVI   K.FCRKMIN,FCRKMINQ                                               
         MVI   K.FCRKTYP,FCRKTYPQ                                               
         MVC   K.FCRKSYS,GCOVSYS   SYSTEM                                       
         MVC   K.FCRKPRG,GCPRGNO   PROGRAM                                      
         MVC   K.FCRKREC,CSREC     RECORD                                       
         MVC   K.FCRKTYPE,GSSMCODE TYPE CODE                                    
*                                                                               
         TM    0(R2),CPAGE         PAGE REQUIRED?                               
         BZ    DCLM04              NO                                           
         MVI   K.FCRKPAGE,FCRKPLST LIST PAGE                                    
         TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST?                            
         BZ    *+10                NO                                           
         MVC   K.FCRKPAGE,GSSMPAGE SCREEN PAGE                                  
*                                                                               
DCLM04   TM    0(R2),CACTION       ACTION REQUIRED?                             
         BZ    *+10                NO                                           
         MVC   K.FCRKACT,CSACT     ACTION                                       
*                                                                               
         TM    0(R2),CSBREC        SUB-RECORD REQUIRED?                         
         BZ    *+10                NO                                           
         MVC   K.FCRKSREC,PSREC    SUB-RECORD                                   
*                                                                               
         TM    0(R2),CAGENCY       AGY ALPHA REQUIRED?                          
         BZ    *+10                NO                                           
         MVC   K.FCRKAGY,CUAALF    AGENCY ALPHA ID                              
*                                                                               
         TM    0(R2),CUSER         PERSON ID REQUIRED?                          
         BZ    *+10                NO                                           
         MVC   K.FCRKPER,CUUSER    PERSON ID                                    
*                                                                               
         MVC   K.FCRKCTRY,CUCTRY   SEE IF COLUMNS FOR CONNECTED COUNTRY         
         XI    K.FCRKCTRY,FF                                                    
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    DCLM08              GOT THIS RECORD?                             
*                                                                               
         XC    K.FCRKEY,K.FCRKEY   TRY TO READ FOR HOST PROCESSOR               
         MVI   K.FCRKMIN,FCRKMINQ                                               
         MVI   K.FCRKTYP,FCRKTYPQ                                               
         MVC   K.FCRKSYS,GCOVSYS   SYSTEM                                       
         MVC   K.FCRKPRG,GCPRGNO   PROGRAM                                      
         MVC   K.FCRKREC,CSREC     RECORD                                       
         MVC   K.FCRKTYPE,GSSMCODE TYPE CODE                                    
*                                                                               
         TM    0(R2),CPAGE         PAGE REQUIRED?                               
         BZ    DCLM06                                                           
         MVI   K.FCRKPAGE,FCRKPLST LIST PAGE                                    
         TM    LSSTAT1,LSSMAIN                                                  
         BZ    *+10                                                             
         MVC   K.FCRKPAGE,GSSMPAGE                                              
*                                                                               
DCLM06   TM    0(R2),CACTION       ACTION REQUIRED?                             
         BZ    *+10                NO                                           
         MVC   K.FCRKACT,CSACT     ACTION                                       
*                                                                               
         TM    0(R2),CSBREC        SUB-RECORD REQUIRED?                         
         BZ    *+10                NO                                           
         MVC   K.FCRKSREC,PSREC    SUB-RECORD                                   
*                                                                               
         TM    0(R2),CAGENCY       AGY ALPHA REQUIRED?                          
         BZ    *+10                NO                                           
         MVC   K.FCRKAGY,CUAALF    AGENCY ALPHA ID                              
*                                                                               
         TM    0(R2),CUSER         PERSON ID REQUIRED?                          
         BZ    *+10                NO                                           
         MVC   K.FCRKPER,CUUSER    PERSON ID                                    
*                                                                               
         MVI   K.FCRKCTRY,FF       TRY FOR HOST PROCESSOR                       
*                                                                               
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    DCLM08              GOT THIS RECORD?                             
*                                                                               
         LA    R2,1(R2)            NEXT CLMTAB ENTRY                            
         B     DCLM02              TRY AGAIN                                    
*                                                                               
CLMTAB   DC    AL1(CPAGE+CACTION+CSBREC+CAGENCY+CUSER)                          
         DC    AL1(CPAGE+CACTION+CSBREC+CAGENCY)                                
         DC    AL1(CPAGE+CACTION+CSBREC)                                        
         DC    AL1(CPAGE+CACTION+CAGENCY+CUSER)                                 
         DC    AL1(CPAGE+CACTION+CAGENCY)                                       
         DC    AL1(CPAGE+CACTION)                                               
         DC    AL1(CPAGE+CAGENCY+CUSER)                                         
         DC    AL1(CPAGE+CAGENCY)                                               
         DC    AL1(CPAGE)                                                       
         DC    AL1(EOT)                                                         
*                                                                               
CPAGE    EQU   X'80'                                                            
CACTION  EQU   X'40'                                                            
CSBREC   EQU   X'20'                                                            
CAGENCY  EQU   X'10'                                                            
CUSER    EQU   X'08'                                                            
*                                                                               
DCLM08   L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                ERROR READING RECORD                         
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,FCRFIRST(R2)                                                  
         USING FCRELD,R2                                                        
         XR    RF,RF                                                            
*                                                                               
DCLM10   CLI   FCREL,0             END OF RECORD?                               
         BE    DCLM20              YES                                          
         CLI   FCREL,FCRELQ        COLUMN ELEMENT?                              
         BE    DCLM14              YES                                          
*                                                                               
DCLM12   IC    RF,FCRLN                                                         
         LA    R2,0(RF,R2)                                                      
         B     DCLM10                                                           
*                                                                               
DCLM14   CLI   FCRTYPE,FCRTFIX     FIXED                                        
         BNE   DCLM16              NO                                           
         MVC   F.DCTINDS1,FCRCIND  SAVE INDICATORS                              
         MVC   F.DCTFLD#,FCRFNUM   SAVE FIELD NUMBER                            
         LH    RE,LSFIXNUM         INCREMENT NUMBER OF FIXED COLUMNS            
         LA    RE,1(RE)                                                         
         STH   RE,LSFIXNUM                                                      
         LA    R3,DCTABL(R3)       NEXT FREE IN FIXED COLUMN BUILD AREA         
         B     DCLM12              CONTINUE ITERATING RECORD                    
*                                                                               
DCLM16   CLI   FCRTYPE,FCRTVAR     VARIABLE?                                    
         BNE   DCLM18              NO                                           
         MVC   V.DCTINDS1,FCRCIND  SAVE INDICATORS                              
         MVC   V.DCTFLD#,FCRFNUM   SAVE FIELD NUMBER                            
         LH    RE,LSVARNUM         INCREMENT NUMBER OF VARIABLE COLUMNS         
         LA    RE,1(RE)                                                         
         STH   RE,LSVARNUM                                                      
         LA    R4,DCTABL(R4)       NEXT FREE IN VARIABLE COLUMN AREA            
         B     DCLM12              CONTINUE ITERATING RECORD                    
*                                                                               
DCLM18   CLI   FCRTYPE,FCRTVOK     VALID FOR DISPLAY                            
         BNE   DCLM12              NO - IGNORE IT                               
         MVC   O.DCTINDS1,FCRCIND  SAVE INDICATORS                              
         MVC   O.DCTFLD#,FCRFNUM   SAVE FIELD NUMBER                            
         LH    RE,LSVALNUM         INCREMENT NUMBER OF VALID COLUMNS            
         LA    RE,1(RE)                                                         
         STH   RE,LSVALNUM                                                      
         LA    R5,DCTABL(R5)       NEXT VALID COLUMN BUILS AREA                 
         B     DCLM12              CONTINUE ITERATING RECORD                    
*                                                                               
DCLM20   OC    LSFIXNUM,LSFIXNUM   COLUMNS SET UP?                              
         BNZ   DCLMX               YES                                          
         OC    LSVARNUM,LSVARNUM                                                
         BNZ   DCLMX               YES                                          
         OC    LSVALNUM,LSVALNUM                                                
         BNZ   DCLMX               YES                                          
*                                                                               
K        USING FDRRECD,IOKEY                                                    
         XC    K.FDRKEY,K.FDRKEY   DEFAULT IS TO USE ALL FIELD RECORDS          
         MVI   K.FDRKMIN,FDRKMINQ  FOR THIS SYS/PROG/RECORD                     
         MVI   K.FDRKTYP,FDRKTYPQ  & TO                                         
         MVC   K.FDRKSYS,GCOVSYS   SET KEY FIELDS FIXED & ALL OTHERS            
         MVC   K.FDRKPRG,GCPRGNO   VARIABLE                                     
         MVC   K.FDRKREC,CSREC                                                  
         GOTOX ('GETFLD',AGROUTS),RTPARM,GFHIGH,AIO1                            
         BE    DCLM24                                                           
         DC    H'0'                                                             
*                                                                               
DCLM22   GOTOX ('GETFLD',AGROUTS),RTPARM,GFRSEQ,AIO1                            
         BNE   DCLMX                                                            
*                                                                               
DCLM24   CLC   K.FDRKEY(FDRKNUM-FDRKEY),IOKEYSAV                                
         BNE   DCLMX                                                            
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,FDRFIRST(R2)                                                  
         USING FDRELD,R2                                                        
         XR    RF,RF                                                            
         CLI   FDREL,FDRELQ                                                     
         BE    *+12                                                             
         IC    RF,FDRLN                                                         
         BXH   R2,RF,*-12                                                       
*                                                                               
         CLI   FDRLCLEN,0          COLUMN WIDTH IS 0 - NOT FOR LIST             
         BE    DCLM22                                                           
         CLI   FDRLVL,FDRIKEY                                                   
         BNE   DCLM28                                                           
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,FDRFIRST(R2)                                                  
DCLM26   CLI   FDREL,0                                                          
         BE    DCLM22                                                           
         CLI   FDREL,FLTRLQ                                                     
         BE    *+12                                                             
         IC    RF,FDRLN                                                         
         BXH   R2,RF,DCLM26                                                     
         TM    FDRINDS1,FDR1REQ                                                 
         BO    DCLM22                                                           
         MVI   F.DCTINDS1,0                                                     
         MVC   F.DCTFLD#,FDRNUM                                                 
         LH    RE,LSFIXNUM                                                      
         LA    RE,1(RE)                                                         
         STH   RE,LSFIXNUM                                                      
         LA    R3,DCTABL(R3)                                                    
         B     DCLM22                                                           
*                                                                               
DCLM28   MVI   V.DCTINDS1,0                                                     
         CLI   CSREC,O#FLTR        FIDDLE FOR FILTER & DISPLAY ROUTINES         
         BE    *+12                                                             
         CLI   CSREC,O#DIS                                                      
         BNE   DCLM30                                                           
         TM    FDRINDS1,FDR1PRO                                                 
         BO    *+8                                                              
         OI    V.DCTINDS1,DCTIOPEN                                              
*                                                                               
DCLM30   MVC   V.DCTFLD#,FDRNUM                                                 
         LH    RE,LSVARNUM                                                      
         LA    RE,1(RE)                                                         
         STH   RE,LSVARNUM                                                      
         LA    R4,DCTABL(R4)                                                    
*                                                                               
DCLMX    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
         DROP  F,V,K,R2,O                                                       
         SPACE 2                                                                
***********************************************************************         
*DELETE TSAR RECORD FROM LIST                                    * 17 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
TSARDEL  L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         GOTOX ('TSARIO',AGROUTS),TSADEL                                        
         LH    RF,LSLST#X          DECREMENT LAST LINE #                        
         BCTR  RF,0                                                             
         STH   RF,LSLST#X                                                       
         CLC   LSLST#1,LSLST#X     TEST LIST NOW EMPTY                          
         BNH   *+8                                                              
         NI    LSLTIND1,FF-LSLTISOL                                             
         XC    TLNUM,TLNUM                                                      
TSDELX   B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
*FIRST FOR UPDATE RECORD FROM LIST                               * 19 *         
*                                                                ******         
* NTRY: P3 = A(FILE RECORD)                                           *         
*       P4 = A(DIRECTORY RECORD)                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
UPDFRST  GOTOX APRG,RTPARM,('GCBOVER',OLIST),LUPDFRST,RTPARMS3,RTPARMS4         
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*LAST FOR UPDATE RECORD FROM LIST                                * 20 *         
*                                                                ******         
* NTRY: P3 = A(FILE RECORD)                                           *         
*       P4 = A(DIRECTORY RECORD)                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
UPDLAST  GOTOX APRG,RTPARM,('GCBOVER',OLIST),LUPDLAST,RTPARMS3,RTPARMS4         
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*UPDATE DIRECTORY RECORD FROM TSAR RECORD                        * 21 *         
*                                                                ******         
* NTRY: P3 = A(DIRECTORY RECORD)                                      *         
*       P4 = A(TSAR RECORD)                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
UPDDIR   GOTOX APRG,RTPARM,('GCBOVER',OLIST),LUPDDIR,RTPARMS3,RTPARMS4          
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*UPDATE FILE RECORD FROM TSAR RECORD                             * 22 *         
*                                                                ******         
* NTRY: P3 = A(FILE RECORD)                                           *         
*       P4 = A(TSAR RECORD)                                           *         
***********************************************************************         
         DS    0H                                                               
         USING *,R6                                                             
UPDREC   GOTOX APRG,RTPARM,('GCBOVER',OLIST),LUPDREC,RTPARMS3,RTPARMS4          
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*ALLOW OVERLAY TO FILTER RECORDS MANUALLY                        * 23 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
TSARTSA  L     R1,LCAR1                                                         
         GOTOX AOLY,(R1)                                                        
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*LKEYCLM                                                         * 24 *         
*SET COLUMNS BASED ON RECORD KEY                                 ******         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*UPDATE RECORDS IN LIST                                          * 25 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
LSTUPD   OC    GSSMPAGE,GSSMPAGE   LIST SCREEN?                                 
         BZ    LUPD02              YES                                          
         TM    GSINDSL1,GSIMLST    MAINTENANCE SCREEN HAS LIST?                 
         BZ    EXITOK              NO                                           
*                                                                               
LUPD02   GOTOX ALST,RTPARM,OLIST,LUPDFRST,AIOREC,GSRECKEY                       
         BL    EXITL               ERROR ON FIRST FOR UPDATE                    
*                                                                               
         TM    LSLTIND1,LSLTISOL   LIST STARTED?                                
         BZ    LUPDX               NO                                           
*                                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         MVC   TLNUM,LSLST#1       FIRST IN THIS PARTICULAR LIST                
         LA    R1,TSAGET                                                        
         B     *+8                                                              
*                                                                               
LUPD04   LA    R1,TSANXT                                                        
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
*                                                                               
         GOTOX ALST,RTPARM,OLIST,LUPDDIR,GSRECKEY,ATLST                         
         BNE   EXITL               ERROR IN DIRECTORY UPDATE                    
         GOTOX (RF),(R1),,LUPDREC,AIOREC,                                       
         BNE   EXITL               ERROR IN FILE UPDATE                         
*                                                                               
         CLC   TLNUM,LSLST#X       REACHED END OF THIS PARTICULAR LIST?         
         BL    LUPD04              NO                                           
*                                                                               
LUPDX    GOTOX ALST,RTPARM,OLIST,LUPDLAST,LCAREC,GSRECKEY                       
         BL    EXITL               ERROR IN LAST FOR UPDATE                     
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
*LSETHEAD                                                        * 26 *         
*SET UP HEADLINES (IF LSS1HEAD)                                  ******         
*                                                                     *         
***********************************************************************         
                                                                                
***********************************************************************         
* SUB-ACTION OBJECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
SUB      DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         LA    R1,SUBTAB                                                        
         USING OBJTABD,R1                                                       
*                                                                               
SUB02    CLI   OBJVERB,EOT                                                      
         BE    SUB08                                                            
         CLC   OBJVERB,LCVERB      R1 HOLDS EQUATED VERB                        
         BE    SUB04               MATCHED                                      
         LA    R1,OBJTABL(R1)                                                   
         B     SUB02               BUMP & LOOP                                  
*                                                                               
SUB04    TM    OBJIND1,OBJPRIV     PRIVATE?                                     
         BZ    SUB06                                                            
         L     RE,4(RD)            MAKE SURE INVOKED AT THIS LEVEL              
         L     RF,4(RE)                                                         
         CLC   16(4,RE),16(RF)                                                  
         BE    SUB06                                                            
         DC    H'0'                                                             
*                                                                               
SUB06    ICM   R6,15,OBJADR                                                     
         A     R6,LCRELO                                                        
         BR    R6                                                               
         DROP  R1                                                               
*                                                                               
SUB08    B     EXITH               NOT KNOWN AT THIS LEVEL                      
*                                                                               
SUBTAB   DS    0A                                                               
         DC    AL1(SAVAL),AL1(0,0,0),AL4(SUBVAL)                                
         DC    AL1(SAMATCH),AL1(0,0,0),AL4(SUBMCH)                              
         DC    AL1(SAPROC),AL1(0,0,0),AL4(SUBPRC)                               
         DC    AL1(SADONE),AL1(0,0,0),AL4(SUBDON)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUB-ACTION INPUT FIELD                                *  1 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
SUBVAL   XC    LSSUBFRA,LSSUBFRA   CLEAR CURRENT SUB-ACTION ELEMENT             
         XC    LSSUBNUM,LSSUBNUM   RESET SUB-ACTION REPEAT COUNT                
*                                                                               
         LH    R2,LSCURLIN         DISPLACEMENT TO CURRENT LINE                 
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
         TM    FHII,FHIIVA         FIELD INPUT?                                 
         BO    *+8                 NO                                           
         OI    LSSCIND1,LSSCIINP   YES - KEEP CURRENT SCREEN                    
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
*                                                                               
         CLI   FVIFLD,C'?'        WANT LIST OF VALID HELP?                      
         BNE   SVAL02                                                           
*                                                                               
         CLI   CSREC,O#SUBAC      CANNOT ALLOW THIS TO GO RECURSIVE             
         BNE   *+8                                                              
         CLI   PSREC,O#SUBAC                                                    
         BE    SVAL02                                                           
         CLI   PSREC,O#RTYP                                                     
         BE    SVAL02                                                           
         CLI   PSREC,O#ACT                                                      
         BE    SVAL02                                                           
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#SUBAC                                                   
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNISEL                                                 
         L     RF,ATLST           PASS CURRENT RECORD MASK                      
         MVC   N.SDATA(L'TLRMSK),TLRMSK-TLSTD(RF)                               
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
         DROP  N                                                                
         DC    H'0'                                                             
*                                                                               
SVAL02   TM    LSSCIND1,LSSCIMEP+LSSCIMEL                                       
         BZ    SVAL04              NO MULTILINE SUB-ACTION                      
*                                                                               
         CLI   FVIFLD,C'*'         IGNORE FIELD MARKER SET?                     
         BE    *+12                YES                                          
         CLI   FVILEN,0            ANY INPUT TO FIELD?                          
         BNE   SVAL04              YES                                          
*                                                                               
         MVC   LSSUBFRA,LSMULFRA   MAKE MULTILINE SUB-ACTION ACTION             
         MVC   LCWORK1,BCSPACES    TRANSLATE ACTION NAME                        
         MVI   LCWORK1,GE#ESCL                                                  
         MVC   LCWORK1+1(L'FRADICT),LSSUBFRA+(FRADICT-FRAELD)                   
         MVC   LCWORK1+3(L'LSSUBLEN),LSSUBLEN                                   
         ICM   RF,15,=CL4'SL  '                                                 
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),LCWORK1                                      
*                                                                               
         XR    RE,RE                                                            
         IC    RE,LSSUBLEN         LENGTH OF SUB-ACTION FIELD                   
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),LCWORK1     DISPLAY MULTILINE ACTION                     
         OI    FHOI,FHOITR         REDISPLAY FIELD                              
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         B     SVAL06              CONTINUE WITH MULTILINE ACTION               
*                                                                               
SVAL04   CLI   LSSUBDEF,C' '       DEFAULT SUB-ACTION SET?                      
         BNH   SVAL10              NO                                           
         CLI   FVILEN,0            INPUT TO FIELD?                              
         BNE   SVAL10              YES                                          
*                                                                               
         XR    RE,RE                                                            
         IC    RE,LSSUBLEN         LENGTH OF SUB-ACTION FIELD                   
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),LSSUBDEF    DISPLAY ONTO LINE DEFAULT ACTION             
         OI    FHOI,FHOITR         REDISPLAY FIELD                              
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
*                                                                               
         GOTOX ALST,RTPARM,OSUBACT,SAMATCH                                      
         BNE   SVAL08              SUB-ACTION CANNOT BE MATCHED                 
*                                                                               
SVAL06   GOTOX ALST,RTPARM,OSUBACT,SAPROC                                       
         BE    SVALOK              SUB-ACTION PROCESSED OK                      
*                                                                               
SVAL08   IC    RE,LSSUBLEN         OUTPUT '*' IF INVALID MULTI/DEFAULT          
         BCTR  RE,0                                                             
         MVC   FHDA(0),LCSPACES                                                 
         MVI   FHDA,C'*'                                                        
         B     SVALOK                                                           
*                                                                               
SVAL10   CLI   FVIFLD,C'-'         STOP MULTI-INPUT FIELDS?                     
         BNE   *+12                NO                                           
         NI    LSSCIND1,FF-(LSSCIMEP+LSSCIMEL+LSSCIMFD)                         
         B     SVALOK                                                           
*                                                                               
         CLI   FVILEN,0            SUB-ACTION ENTERED?                          
         BE    SVALOK              NO                                           
         CLI   FVIFLD,C'*'         IGNORE THIS FIELD?                           
         BE    SVALOK              YES                                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         BZ    SVAL20              ONLY 1 CHARACTER INPUT                       
         LA    RF,FVIFLD(RE)       TEST SUFFIX CHARACTER                        
*                                                                               
         CLC   LSSSEOP,0(RF)       SELECT TO END-OF-PAGE?                       
         BNE   *+8                                                              
         OI    LSSCIND1,LSSCIMEP                                                
*                                                                               
         CLC   LSSSEOL,0(RF)       SELECT TO END-OF-LIST?                       
         BNE   *+8                                                              
         OI    LSSCIND1,LSSCIMEL                                                
*                                                                               
         TM    LSSCIND1,LSSCIMEP+LSSCIMEL                                       
         BZ    SVAL12              MULTILINE INPUT NOT SET                      
*                                                                               
         MVI   0(RF),C' '          ERASE LAST CHARACTER                         
         STC   RE,FVILEN           RESET LENGTHS                                
         BCTR  RE,0                                                             
         STC   RE,FVXLEN                                                        
         BCTR  RF,0                                                             
*                                                                               
SVAL12   CLI   0(RF),C'0'          NUMERICAL ENDING?                            
         BL    SVAL20              NO                                           
         CLI   0(RF),C'9'                                                       
         BH    SVAL20              NO                                           
*                                                                               
         XR    R1,R1               R1 HOLDS LENGTH                              
SVAL14   CLI   0(RF),C'0'          STILL NUMERIC?                               
         BL    SVAL16              NO                                           
         CLI   0(RF),C'9'                                                       
         BH    SVAL16              NO                                           
*                                                                               
         LA    R1,1(R1)                                                         
         BCTR  RF,0                                                             
         BCT   RE,SVAL14                                                        
*                                                                               
SVAL16   BCTR  R1,0                                                             
         EX    R1,SVALPAK          OBTAIN PACKED NUMBER                         
         CVB   R0,GCDUB1                                                        
         EX    R1,SVALMVE          CLEAR NUMBER FROM INPUT FIELD                
         B     SVAL18                                                           
*                                                                               
SVALPAK  PACK  GCDUB1,1(0,RF)      PACK NUMBER INTO GCDUB1                      
SVALMVE  MVC   1(0,RF),BCSPACES    CLEAR NUMERIC PORTION OF FIELD               
*                                                                               
SVAL18   STH   R0,LSSUBNUM         SAVE MULTILINE ACTION REPEAT NUMBER          
         XR    R0,R0                                                            
         IC    R0,FVILEN           REVALIDATE FVIFLD                            
         GOTOX ('FLDVAL',AGROUTS),0                                             
*                                                                               
SVAL20   GOTOX ALST,RTPARM,OSUBACT,SAMATCH                                      
         BNE   SVALL               NO MATCH FOR THIS INPUT                      
*                                                                               
         PUSH  USING                                                            
         USING FRAELD,LSSUBFRA                                                  
*        TM    LSSCIND1,LSSCIMEP   END-OF-PAGE VALID?                           
*        BZ    *+12                                                             
*        TM    SELTIND1,SELTIEOP                                                
*        BZ    SVALL                                                            
*                                                                               
*        TM    LSSCIND1,LSSCIMEL   END-OF-LIST VALID?                           
*        BZ    *+12                                                             
*        TM    SELTIND1,SELTIEOL                                                
*        BZ    SVALL                                                            
*                                                                               
         TM    LSSCIND1,LSSCIMEP+LSSCIMEL                                       
         BZ    *+10                                                             
         MVC   LSMULFRA,LSSUBFRA   SAVE MULTILINE ACTION ELEMENT                
         POP   USING                                                            
*                                                                               
         GOTOX ALST,RTPARM,OSUBACT,SAPROC                                       
         BNE   SVALL               ERROR ON PROCESS OF SUB-ACTION               
*                                                                               
SVALOK   OI    FHII,FHIIVA                                                      
         B     EXITOK                                                           
SVALL    B     EXITL                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* MATCH INPUT TO SUB-ACTION                                      *  2 *         
*                                                                ******         
* NTRY: FVIFLD HOLDS VALIDATED SUB-ACTION FIELD                       *         
* EXIT: LSSUBFRA = VALID ACTION ELEMENT                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
X        USING FRARECD,IOKEY                                                    
SUBMCH   XC    X.FRAKEY,X.FRAKEY   BUILD ACTION RECORD                          
         MVI   X.FRAKMIN,FRAKMINQ                                               
         MVI   X.FRAKTYP,FRAKTYPQ                                               
         MVC   X.FRAKSYS,GCOVSYS   SYSTEM                                       
         MVC   X.FRAKPRG,GCPRGNO   PROGRAM                                      
         MVC   X.FRAKREC,CSREC     RECORD                                       
         MVC   BCWORK,IOKEY                                                     
*                                                                               
SUBMCH02 GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         MVC   IOKEY,BCWORK                                                     
         XR    RF,RF               INCREMENT RECORD NUMBER                      
         IC    RF,X.FRAKACT                                                     
         LA    RF,1(RF)                                                         
         STC   RF,X.FRAKACT                                                     
*                                                                               
         L     R1,=AL4(XOGENDIR+XOHIGH+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(FRAKACT-FRARECD),IOKEYSAV                                  
         BE    SUBMCH04                                                         
         B     SUBMCHN                                                          
*&&NOP                                                                          
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#SUBACT     RECORD HELP IS IN GEFILREC                   
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNISEL    MAKE THIS A 'SELECT' ACTION                  
         XC    LCBYTE,LCBYTE                                                    
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
         DC    H'0'                IF YOU GET HERE YOU ARE IN TROUBLE           
         DROP  N                                                                
*&&                                                                             
SUBMCH04 MVC   BCWORK,IOKEY        SAVE THIS KEY                                
         XR    R1,R1                                                            
         ICM   R1,8,X.FRAKSYS                                                   
         ICM   R1,4,X.FRAKPRG                                                   
         ICM   R1,2,X.FRAKREC                                                   
         ICM   R1,1,X.FRAKACT                                                   
         GOTOX ('GETFACTN',AGROUTS),(R1)                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         LA    R2,FRAFIRST(R2)                                                  
         USING FRAELD,R2                                                        
         XR    RF,RF                                                            
SUBMCH06 CLI   FRAEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FRAEL,FRAELQ                                                     
         BE    SUBMCH08                                                         
         IC    RF,FRALN                                                         
         AR    R4,RF                                                            
         B     SUBMCH04                                                         
*                                                                               
SUBMCH08 TM    FRAINDS1,FRA1SUB    SUB-ACTION?                                  
         BZ    SUBMCH02            NO                                           
*                                                                               
         XR    RF,RF                                                            
         NI    X.FRAKSTAT,FRAKSVAL TURN OFF ALL BUT MINIMUM INPUT LEN.          
         ICM   RF,1,X.FRAKSTAT                                                  
         SRL   RF,3                                                             
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,1                MINIMUM LENGTH DEFAULT IS 1                  
         CLM   RF,1,FVILEN                                                      
         BH    SUBMCH02            NOT ENOUGH CHARS INPUT FOR THIS ONE          
*                                                                               
         MVC   LCWORK1,BCSPACES                                                 
         MVI   LCWORK1,GE#ESCL                                                  
         MVC   LCWORK1+1(L'FRADICT),FRADICT                                     
         MVC   LCWORK1+3(L'LSSUBLEN),LSSUBLEN                                   
         ICM   RF,15,=CL4'SU  '                                                 
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),LCWORK1                                      
*                                                                               
         MVI   LCWORK1+10,GE#ESCL                                               
         MVC   LCWORK1+11(L'FRADICT),X.FRAKDICT                                 
         MVC   LCWORK1+13(L'LSSUBLEN),LSSUBLEN                                  
         ICM   RF,15,=CL4'SL  '                                                 
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),LCWORK1+10                                   
*                                                                               
         XR    RF,RF               TRY TO MATCH UPPERCASE NAME                  
         IC    RF,FVILEN                                                        
         CLM   RF,1,=AL1(SLEN#Q)   LEN IS > THAN REQUIREMENT?                   
         BNH   *+8                 NO                                           
         LA    RF,SLEN#Q                                                        
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8              MATCH NAME AGAINST ACTION NAME               
         BE    SUBMCH12            MATCH                                        
         CLC   FVIFLD(0),LCWORK1                                                
*                                                                               
         EX    RF,*+8              MATCH NAME AGAINST ACTION NAME               
         BE    SUBMCH12            MATCH                                        
         CLC   FVIFLD(0),LCWORK1+10                                             
         B     SUBMCH02                                                         
*                                                                               
SUBMCH12 GOTOX AGEN,RTPARM,OACT,ATST,AIO1                                       
         BNE   SUBMCH02            ACTION IS NOT VALID                          
*                                                                               
         LA    RE,TWASESRA         RECORD/ACTION ALREADY USED?                  
         XR    RF,RF                                                            
         ICM   RF,1,TWASESNL       NEST LEVEL                                   
         BCT   RF,*+8                                                           
         B     SUBMCH14            NO - SAFE TO USE RECORD/ACTION               
                                                                                
         CLC   FRANREC(2),0(RE)                                                 
         BE    SUBMCH02            YES - DO NOT REUSE IT THEN                   
         LA    RE,L'TWASESRA(RE)                                                
         BCT   RF,*-14                                                          
*                                                                               
SUBMCH14 MVC   LSSUBFRA,0(R2)      SAVE SUB-ACTION ELEMENT                      
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         MVC   GCFULL1,GSRECMSK    SAVE GSRECMSK                                
         L     RE,ATLST            SET MASK FOR TEST                            
         MVC   GSRECMSK,TLRMSK-TLSTD(RE)                                        
         XR    RF,RF                                                            
         ICM   RF,8,=AL1(ACTTST)                                                
         ICM   RF,1,FRAVERB                                                     
         GOTOX AGEN,RTPARM,OACTH,(RF)                                           
         MVC   GSRECMSK,GCFULL1    RESTORE GSRECMSK                             
         BL    SUBMCH02            ACTION IS NOT VALID                          
         B     EXITOK                                                           
*                                                                               
SUBMCHN  MVC   FVMSGNO,=AL2(GE$INACT)                                           
         MVC   FVXTRA,FVIFLD                                                    
         XC    LSSUBFRA,LSSUBFRA   RESET SUB-ACTION ELEMENT                     
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITL                                                            
*                                                                               
         DROP  X,R2                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS CURRENT SUB-ACTION                                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
SUBPRC   XR    RF,RF                                                            
         IC    RF,LSSUBFRA+(FRASACT-FRAELD)                                     
         GOTOX ALST,RTPARM,OSUBH,(RF)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SUB-ACTION SUCCESSFULLY PROCESSED                                   *         
***********************************************************************         
         SPACE 1                                                                
SUBDON   DS    0H                                                               
         USING *,R6                                                             
         CLI   LSSUBLEN,0          DON'T DO THIS IF NO SUB-ACT FIELD            
         BE    EXITOK                                                           
*                                                                               
         MVC   LCWORK1,BCSPACES                                                 
         MVI   LCWORK1,GE#ESCL                                                  
         MVC   LCWORK1+1(L'FRADICT),LSSUBFRA+(FRADICT-FRAELD)                   
         MVC   LCWORK1+3(L'LSSUBLEN),LSSUBLEN                                   
         ICM   RF,15,=CL4'SL  '                                                 
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),LCWORK1                                      
*                                                                               
         LH    R2,LSCURLIN                                                      
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
         MVI   FHDA,C'*'                                                        
         IC    RE,LSSUBLEN                                                      
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FHDA+1(0),LCWORK1                                                
         OI    FHII,FHIIVA                                                      
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ACTION HANDLER OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SHAC     LA    R1,SHACTAB                                                       
         USING OBJTABD,R1                                                       
*                                                                               
SHAC02   CLI   OBJVERB,EOT                                                      
         BE    SHAC08                                                           
         CLC   OBJVERB,LCVERB      R1 HOLDS EQUATED VERB                        
         BE    SHAC04              MATCHED                                      
         LA    R1,OBJTABL(R1)                                                   
         B     SHAC02              BUMP & LOOP                                  
*                                                                               
SHAC04   TM    OBJIND1,OBJPRIV     PRIVATE?                                     
         BZ    SHAC06                                                           
         L     RE,4(RD)            MAKE SURE INVOKED AT THIS LEVEL              
         L     RF,4(RE)                                                         
         CLC   16(4,RE),16(RF)                                                  
         BE    SHAC06                                                           
         DC    H'0'                                                             
*                                                                               
SHAC06   ICM   R6,15,OBJADR                                                     
         A     R6,LCRELO                                                        
         BR    R6                                                               
         DROP  R1                                                               
*                                                                               
SHAC08   B     EXITH               NOT KNOWN AT THIS LEVEL                      
*                                                                               
SHACTAB  DS    0A                                                               
         DC    AL1(SHNTR),AL1(0,0,0),AL4(SHANTR)                                
         DC    AL1(SHSEL),AL1(0,0,0),AL4(SHASEL)                                
         DC    AL1(SHINS),AL1(0,0,0),AL4(SHAINS)                                
         DC    AL1(SHUSER),AL1(0,0,0),AL4(SHAUSR)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* NTRSES INTO NEW RECORD/ACTION                                       *         
***********************************************************************         
         SPACE 1                                                                
SHANTR   DS    0H                                                               
         USING *,R6                                                             
         GOTOX AOLY,RTPARM,OSUBH,SACHECK,LSSUBFRA,ATLST                         
         BL    EXITL                                                            
*                                                                               
         GOTOX AGETREC,0            GET TSAR & FILE RECORDS                     
*                                                                               
         XR    RF,RF                TEST SUB-ACTION VALIDITY                    
         ICM   RF,1,LSSUBFRA+FRANACT-FRAELD                                     
         MVC   GCFULL1,GSRECMSK                                                 
         L     RE,ATLST                                                         
         MVC   GSRECMSK,TLRMSK-TLSTD(RE)                                        
         GOTOX AGEN2,RTPARM,OACTH,('ACTTST',(RF))                               
         MVC   GSRECMSK,GCFULL1                                                 
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFESEL)                                            
         B     EXITL                                                            
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVC   N.SRECACT,LSSUBFRA+(FRANREC-FRAELD)                              
         OI    N.SNINDS1,SNIUSECR                                               
         LH    R0,LSSUBNUM                                                      
         STC   R0,N.SMPAGE                                                      
         GOTOX AGEN,RTPARM,OSES,SNTR                                            
         DROP  N                                                                
         EJECT                                                                  
***********************************************************************         
* SELECT                                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
SHASEL   TM    LSSTAT1,LSSSEL      THIS ITEM SELECTED?                          
         BZ    EXITL               NO                                           
         OI    P.SXINDS1,SXISEL    SET SELECTED                                 
         GOTOX AGETREC,0           GET RECORD FROM FILE                         
         GOTOX AGEN,RTPARM,OSES,SXIT                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* INSERT INTO  LINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
SHAINS   L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         L     R3,AIO1                                                          
TEMP     USING TLSTD,R3                                                         
*                                                                               
         MVI   LCBYTE,0            RESET DUPLICATED RECORD FLAG                 
         XC    TLKEY,TLKEY         RESET RECORD KEY                             
         MVC   TLKSES,TWASESNL     SET SESSION NEXT LEVEL                       
         MVC   TLNUM,LSLINE#       THIS LINE NUMBER                             
         LA    R1,TSAGET           GET THIS RECORD                              
         B     *+8                                                              
SINS02   LA    R1,TSARDH           READ HIGH FOR RECORD KEY                     
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SINS06              END OF FILE                                  
*                                                                               
SINS04   CLC   TLKSES,TWASESNL     CORRECT SESSION STILL?                       
         BNE   SINS06              NO                                           
*                                                                               
         L     R0,AIO1             COPY RECORD TO TEMP.TLSTD                    
         L     RE,ATLST                                                         
         LH    R1,=Y(L'TLST)                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOX ALST,RTPARM,OLIST,LTSARDEL  DELETE RECORD FROM LIST              
*                                                                               
         MVC   TLKEY,TEMP.TLKEY    RESTORE KEY AFTER DELETE                     
         XC    TLNUM,TLNUM                                                      
*                                                                               
         MVI   TEMP.TLKSES,TLKSTEMP  SET LEVEL TO FF & ADD RECORD               
         XC    TEMP.TLNUM,TEMP.TLNUM                                            
         GOTOX  ('TSARIO',AGROUTS),RTPARM,('TSAADD',TEMP.TLSTD)                 
         B     SINS02                                                           
*                                                                               
SINS06   XC    TEMP.TLKEY,TEMP.TLKEY                                            
         XC    TEMP.TLNUM,TEMP.TLNUM                                            
         MVI   TEMP.TLKSES,TLKSTEMP                                             
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSARDH',TEMP.TLSTD)                  
         BL    SINS12              END OF FILE                                  
         CLI   TEMP.TLKSES,TLKSTEMP                                             
         BNE   SINS12              NO MORE TEMPORARY RECORDS                    
*                                                                               
         L     R0,AIO1             COPY THIS RECORD                             
         L     RE,ATLST                                                         
         LH    R1,=Y(L'TLST)                                                    
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSADEL',TEMP.TLSTD)                  
*                                                                               
         CLI   LCBYTE,FF           DID WE DUPLICATE FIRST RECORD?               
         BE    SINS08              YES                                          
*                                                                               
         L     R0,AIO1             SAVE THE RECORD                              
         L     RE,ATLST                                                         
         LH    R1,=Y(L'TLST)                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
SINS08   XC    TLNUM,TLNUM                                                      
         MVC   TLKSES,TWASESNL                                                  
         GOTOX ALST,RTPARM,OLIST,LTSARADD                                       
         BE    SINS10                                                           
         MVC   FVMSGNO,=AL2(GE$TOMNY)                                           
         B     EXITL                                                            
*                                                                               
SINS10   CLI   LCBYTE,FF           DID WE DUPLICATE FIRST RECORD?               
         BE    SINS06              YES                                          
         MVI   LCBYTE,FF           DUPLICATE FIRST RECORD                       
         L     R0,AIO1             RESTORE THE RECORD                           
         L     RE,ATLST                                                         
         LH    R1,=Y(L'TLST)                                                    
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         B     SINS08                                                           
*                                                                               
SINS12   GOTOX ASETPAG                                                          
         GOTOX ADISPAG                                                          
         LH    R1,LSCURLIN                                                      
         A     R1,ATWA                                                          
         GOTOX AVALNLIN,(R1)                                                    
         B     EXITOK                                                           
         DROP  TEMP                                                             
         EJECT                                                                  
***********************************************************************         
* LET APPLICATION PROCESS SUB-ACTION                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
SHAUSR   GOTOX AOLY,RTPARM,OSUBACT,SHUSER                                       
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SCROLL FIELD OBJECT                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         LA    R1,SCRTAB                                                        
         USING OBJTABD,R1                                                       
*                                                                               
SCR02    CLI   OBJVERB,EOT                                                      
         BE    SCR08                                                            
         CLC   OBJVERB,LCVERB      R1 HOLDS EQUATED VERB                        
         BE    SCR04               MATCHED                                      
         LA    R1,OBJTABL(R1)                                                   
         B     SCR02               BUMP & LOOP                                  
*                                                                               
SCR04    TM    OBJIND1,OBJPRIV     PRIVATE?                                     
         BZ    SCR06                                                            
         L     RE,4(RD)            MAKE SURE INVOKED AT THIS LEVEL              
         L     RF,4(RE)                                                         
         CLC   16(4,RE),16(RF)                                                  
         BE    SCR06                                                            
         DC    H'0'                                                             
*                                                                               
SCR06    ICM   R6,15,OBJADR                                                     
         A     R6,LCRELO                                                        
         BR    R6                                                               
         DROP  R1                                                               
*                                                                               
SCR08    B     EXITH               NOT KNOWN AT THIS LEVEL                      
*                                                                               
SCRTAB   DS    0A                                                               
         DC    AL1(SCVAL),AL1(0,0,0),AL4(SCRVAL)                                
         DC    AL1(SCDIS),AL1(0,0,0),AL4(SCRDIS)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
SCRDICT  DS    0X                                                               
         DCDDL GE#PAGE2,8,L                                                     
         DCDDL GE#MAX,8,L                                                       
         DCDDL GE#HALF,8,L                                                      
SCRDICTX DC    AL1(EOT)                                                         
         SPACE 1                                                                
SDTAB    DS    0XL5                                                             
         DC    AL1(GSKIPAGE),AL2(UC@PAGE-LCWORK1,LC@PAGE-LCWORK1)               
         DC    AL1(GSKIMAXN),AL2(UC@MAX-LCWORK1,LC@MAX-LCWORK1)                 
         DC    AL1(GSKIHALF),AL2(UC@HALF-LCWORK1,LC@HALF-LCWORK1)               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SCROLL FIELD                                    *         
*                                                                     *         
* EXIT: BCSRNUM SET UP                                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R6                                                             
SCRVAL   OC    GSDSPSCR,GSDSPSCR   SCROLL FIELD ON SCREEN                       
         BNZ   *+12                YES                                          
         MVI   GSSCRNUM,GSKIPAGE                                                
         B     SCRVALY                                                          
*                                                                               
         LH    R2,GSDSPSCR         R2=A(SCROLL FIELD)                           
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
*                                                                               
         NI    GSSCRNUM,FF-GSKIMAXN                                             
*                                                                               
         TM    FHII,FHIIVA         FIELD PREVIOUSLY VALIDATED?                  
         BZ    *+14                NO                                           
         OC    GSSCRNUM,GSSCRNUM   MAKE SURE VALID SCROLL AMOUNT                
         BNZ   SCRVALY             YES                                          
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         BNL   *+12                                                             
         MVI   GSSCRNUM,GSKIPAGE                                                
         B     SCRVAL10                                                         
*                                                                               
         TM    FVIIND,FVINUM       TEST NUMERICAL INPUT                         
         BZ    SCRVAL02                                                         
         OC    BCFULL,BCFULL       VALIDATE SCROLL AMOUNT                       
         BZ    SCRVALN                                                          
         OC    BCFULL(3),BCFULL                                                 
         BNZ   SCRVALN                                                          
         CLI   BCFULL+3,15                                                      
         BH    SCRVALN                                                          
         MVC   GSSCRNUM,BCFULL+3                                                
         B     SCRVAL10                                                         
*                                                                               
SCRVAL02 GOTOX VDICTAT,RTPARM,C'LU  ',SCRDICT,UCSCR                             
         GOTOX (RF),(R1),C'LL  ',,LCSCR                                         
*                                                                               
         LA    R3,SDTAB            R3=A(SCROLL DICTIONARY TABLE)                
         IC    RE,FVXLEN                                                        
SCRVAL04 LH    R1,1(R3)                                                         
         LA    R1,LCWORK1(R1)                                                   
         EX    RE,*+8                                                           
         BE    SCRVAL06                                                         
         CLC   FVIFLD(0),0(R1)                                                  
         LH    R1,3(R3)                                                         
         LA    R1,LCWORK1(R1)                                                   
         EX    RE,*+8                                                           
         BE    SCRVAL06                                                         
         CLC   FVIFLD(0),0(R1)                                                  
         LA    R3,L'SDTAB(R3)                                                   
         CLI   0(R3),EOT                                                        
         BNE   SCRVAL04                                                         
         B     SCRVALN                                                          
*                                                                               
SCRVAL06 MVC   GSSCRNUM,0(R3)                                                   
         CLI   GSSCRNUM,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
SCRVAL10 GOTOX ALST,RTPARM,OSCROLL,SCDIS                                        
*                                                                               
SCRVALY  CLI   GSSCRNUM,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EXITOK                                                           
*CRVALY  B     EXITOK                                                           
*                                                                               
SCRVALN  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   GSSCRNUM,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SCROLL FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRDIS   DS    0H                                                               
         USING *,R6                                                             
         OC    GSDSPSCR,GSDSPSCR   SCROLL FIELD?                                
         BZ    EXITOK              NO                                           
*                                                                               
         LH    R2,GSDSPSCR         R2=A(SCROLL FIELD)                           
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
*                                                                               
         XR    R3,R3                                                            
         IC    R3,FHLN                                                          
         SH    R3,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    R3,FHDAD(R3)                                                     
         EX    R3,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
*                                                                               
         TM    GSSCRNUM,GSKIMAXN+GSKIPAGE+GSKIHALF                              
         BNZ   SDIS02              NAME SCROLL                                  
*                                                                               
         XR    RE,RE                                                            
         IC    RE,GSSCRNUM                                                      
         CVD   RE,LCDUB                                                         
         OI    LCDUB+7,X'0F'                                                    
         UNPK  FHDA(2),LCDUB                                                    
         CLI   FHDA,C'0'                                                        
         BNE   *+10                                                             
         MVC   FHDA(2),FHDA+1                                                   
         B     SCRDISX                                                          
*                                                                               
SDIS02   GOTOX VDICTAT,RTPARM,C'LL  ',SCRDICT,LCSCR                             
         LA    RE,LC@PAGE                                                       
         TM    GSSCRNUM,GSKIHALF                                                
         BZ    *+8                                                              
         LA    RE,LC@HALF                                                       
         EX    R3,*+4                                                           
         MVC   FHDA(0),0(RE)                                                    
*                                                                               
SCRDISX  OI    FHOI,FHOITR                                                      
         NI    FHII,FF-FHIIVA                                                   
         TM    GSSCRNUM,GSKIMAXN                                                
         BNZ   *+8                                                              
         OI    FHII,FHIIVA                                                      
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
*** LIST ROUTINES                                                   ***         
***********************************************************************         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO CONTROL THE LIST                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CONT     TM    LSSTAT2,LSTSFULL    TEST TSAR BUFFER FULL                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$TOMNY)                                           
         B     EXITL                                                            
*                                                                               
         TM    LSSCIND1,LSSCICLM   COLUMNS HAVE CHANGED?                        
         BZ    CONT02              NO                                           
*                                                                               
         XC    LSVARLHS,LSVARLHS   RESET LHS COLUMN NUMBER                      
         GOTOX ABLDSCR,BSWRIGHT    REBUILD SCREEN FROM LEFT - RIGHT             
         B     CONT04              CONTINUE                                     
*                                                                               
CONT02   CLI   GSFRP.FRPTYPE,FRPTLEFT HORIZONTAL SCROLL REQUEST?                
         BE    *+12                                                             
         CLI   GSFRP.FRPTYPE,FRPTRGHT                                           
         BNE   CONT04                                                           
*                                                                               
         GOTOX ASCRHAMT            VALIDATE HORIZONTAL SCROLL AMOUNT            
         GOTOX ASCRHORZ            SCROLL HORIZONTALLY                          
*                                                                               
CONT04   GOTOX AGEN2,RTPARM,OPAGE,PGVAL                                         
         LH    RF,GSDSPACT                                                      
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
*                                                                               
         GOTOX ASCRVAMT            VALIDATE VERTICAL SCROLL AMOUNT              
*                                                                               
         TM    LSLTIND1,LSLTIBLD   LIST BUILD BEGUN?                            
         BZ    CONT06              NO - BUILD LIST FROM START                   
         TM    LSSCIND1,LSSCIFLT   FILTERS CHANGED?                             
         BO    CONT06              YES - REBUILD LIST FROM START                
         CLI   GSFRP.FRPTYPE,FRPTRFRS  LIST REFRESH REQUEST?                    
         BE    CONT06              YES - REBUILD LIST FROM START                
         TM    LSLTIND1,LSLTIDIS   LIST DISPLAYED?                              
         BZ    CONT08              NO - DISPLAY PAGE 1                          
         B     CONT10              CONTINUE FROM PREVIOUS BUILD POINT           
*                                                                               
CONT06   NI    LSLTIND1,FF-(LSLTISOL+LSLTIBLD)  TURN OFF LIST STARTED           
         OI    LSSCIND1,LSSCIFLT   SET FILTERS CHANGED                          
*                                                                               
         LH    R1,LSLST#1          SET FIRST NUMBER FOR THIS LIST               
         AH    R1,LSLINPAG         NUMBER OF LIST ITEMS ON A PAGE               
         GOTOX ABLDLST,(R1)        BUILD PAGE 1 AT LEAST                        
         BL    EXITL                                                            
*                                                                               
CONT08   MVC   LSPAG#1,LSLST#1     SET PAGE INFO                                
         GOTOX ASETPAG                                                          
         MVC   LSLINE#,LSPAG#1     DISPLAY PAGE 1                               
*                                                                               
         TM    LSLTIND1,LSLTIFVL   FORCE VALPAG                                 
         BO    CONT16                                                           
*                                                                               
         GOTOX ADISPAG                                                          
         B     CONT20                                                           
*                                                                               
CONT10   CLI   GSFRP.FRPTYPE,FRPTUP TEST FOR VERTICAL SCROLL PFKEY              
         BE    CONT11                                                           
         CLI   GSFRP.FRPTYPE,FRPTMUP                                            
         BE    CONT11                                                           
         CLI   GSFRP.FRPTYPE,FRPTDOWN                                           
         BE    CONT11                                                           
         CLI   GSFRP.FRPTYPE,FRPTMDWN                                           
         BE    CONT11                                                           
         B     CONT12              NO VERTICAL SCROLL                           
*                                                                               
CONT11   LH    R1,LCVERSCR         VERTICAL SCROLL AMOUNT                       
*                                                                               
         CLI   GSFRP.FRPTYPE,FRPTMUP                                            
         BE    *+12                                                             
         CLI   GSFRP.FRPTYPE,FRPTMDWN                                           
         BNE   *+8                                                              
         OI    GSSCRNUM,GSKIMAXN   SET MAXIMUM SCROLL                           
*                                                                               
         TM    GSSCRNUM,GSKIMAXN   TEST FOR MAXIMUM SCROLL                      
         BZ    *+6                                                              
         XR    R1,R1                                                            
         L     RF,ASCRDOWN         SCROLL DOWN                                  
*                                                                               
         CLI   GSFRP.FRPTYPE,FRPTUP                                             
         BE    *+12                                                             
         CLI   GSFRP.FRPTYPE,FRPTMUP                                            
         BNE   *+8                                                              
         L     RF,ASCRUP           SCROLL UP                                    
*                                                                               
         GOTOX (RF),(R1)                                                        
*                                                                               
         MVC   LSLINE#,LSPAG#1     DISPLAY NEW PAGE                             
         GOTOX ADISPAG                                                          
         B     CONT20                                                           
*                                                                               
CONT12   CLI   GSFRP.FRPTYPE,FRPTLEFT  HORIZONTAL SCROLL REQUEST?               
         BE    CONT14                                                           
         CLI   GSFRP.FRPTYPE,FRPTRGHT                                           
         BE    CONT14                                                           
         TM    LSSCIND1,LSSCICLM   COLUMNS HAVE CHANGED?                        
         BZ    CONT16              NO                                           
*                                                                               
CONT14   MVC   LSLINE#,LSPAG#1     RE-DISPLAY CURRENT PAGE                      
         GOTOX ADISPAG                                                          
         B     CONT20                                                           
*                                                                               
CONT16   MVC   LSLINE#,LSPAG#1     VALIDATE LINES ON PAGE                       
         GOTOX AVALPAG                                                          
         BNE   EXITL               ERROR IN LINE VALIDATION                     
*                                                                               
         TM    LSSTAT3,LS3NOSCR    FORCE NOT TO SCROLL?                         
         BZ    CONT18                                                           
         GOTOX ASETPAG                                                          
         MVC   LSLINE#,LSPAG#1     REDISPLAY THIS PAGE                          
         GOTOX ADISPAG                                                          
         NI    LSSTAT3,FF-(LS3NOSCR)                                            
         B     CONTX                                                            
*                                                                               
CONT18   TM    LSSCIND1,LSSCIINP   INPUT TO SCREEN?                             
         BNZ   CONT20              YES                                          
*                                                                               
         LH    R1,LCVERSCR         SCROLL DOWN                                  
         TM    GSSCRNUM,GSKIMAXN                                                
         BZ    *+6                                                              
         XR    R1,R1                                                            
         GOTOX ASCRDOWN,(R1)                                                    
*                                                                               
         MVC   LSLINE#,LSPAG#1                                                  
         GOTOX ADISPAG                                                          
*                                                                               
CONT20   TM    LSSCIND1,LSSCIBLD   REBUILD LIST?                                
         BZ    CONT22              NO                                           
         OI    LSSCIND1,LSSCIFLT                                                
         NI    LSLTIND1,FF-LSLTIBLD                                             
         LH    R1,LSLST#1                                                       
         AH    R1,LSLINPAG                                                      
         GOTOX ABLDLST,(R1)        BUILD AT LEAST PAGE 1                        
         BL    EXITL                                                            
*                                                                               
         MVC   LSPAG#1,LSLST#1     SET PAGE INFO                                
         GOTOX ASETPAG                                                          
         MVC   LSLINE#,LSPAG#1     DISPLAY PAGE 1                               
         GOTOX ADISPAG                                                          
         B     CONTX                                                            
*                                                                               
CONT22   TM    LSLTIND1,LSLTISHU   TSAR SHUFFLE REQUIRED?                       
         BZ    CONT24              NO                                           
         GOTOX ASHUFFLE                                                         
         BNE   EXIT                                                             
         B     CONTX                                                            
*                                                                               
CONT24   TM    LSSCIND2,LSSCIRED   RE-DISPLAY LIST?                             
         BZ    CONT26              NO                                           
         MVC   LSPAG#1,LSLST#1     SET PAGE INFO                                
         GOTOX ASETPAG                                                          
         MVC   LSLINE#,LSPAG#1     DISPLAY PAGE 1                               
         GOTOX ADISPAG                                                          
         B     CONTX                                                            
*                                                                               
CONT26   TM    LSSCIND2,LSSCIPAG   RE-DISPLAY THIS PAGE                         
         BZ    CONTX               NO                                           
         GOTOX ASETPAG                                                          
         MVC   LSLINE#,LSPAG#1     DISPLAY PAGE 1                               
         GOTOX ADISPAG                                                          
*                                                                               
CONTX    OI    LSSCIND2,LSSCIOK                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET LIST MESSAGE                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
LSTMSG   L     RE,ATWA             LIST INPUT FIELD                             
         AH    RE,LS1STINP                                                      
         ST    RE,FVADDR                                                        
*                                                                               
         TM    GSINDSL3,GSI2LMSG   SET OWN MESSAGE?                             
         BO    LSTMSGX             YES                                          
*                                                                               
         TM    LSSTAT2,LSTSFULL    TEST TSAR BUFFER FULL                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$TOMNY)                                           
         B     LSTMSGX                                                          
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         TM    LSSCIND1,LSSCIBLD   TEST LIST RE-BUILT                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GI$LIREF)                                           
         B     LSTMSGX                                                          
*                                                                               
         TM    LSLTIND1,LSLTISOL   TEST ANYTHING IN LIST                        
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(GI$NOREC)                                           
         B     LSTMSGX                                                          
*                                                                               
         TM    LSLTIND1,LSLTIEOL   TEST LAST RECORD DISPLAYED                   
         BZ    LMSG02                                                           
         CLC   LSPAG#X,LSLST#X                                                  
         BL    LMSG02                                                           
*                                                                               
         CLC   LSPAG#1,LSLST#1     TEST FIRST RECORD DISPLAYED                  
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(GI$ALLRD)                                           
         B     LSTMSGX                                                          
*                                                                               
         LH    RE,LCVERSCR                                                      
         TM    GSSCRNUM,GSKIHALF                                                
         BZ    *+8                                                              
         LA    RE,1                                                             
         AH    RE,LSPAG#1                                                       
         CH    RE,LSLST#X                                                       
         BNH   LMSG02                                                           
         MVC   FVMSGNO,=AL2(GI$EOLEF)                                           
         B     LSTMSGX                                                          
*                                                                               
LMSG02   MVC   FVMSGNO,=AL2(GI$DISNX)                                           
         TM    LSSCIND1,LSSCINUP                                                
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(GI$STDIS)                                           
*                                                                               
LSTMSGX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SCROLL DOWN                                              *         
*                                                                     *         
* NTRY: R1 = SCROLL AMOUNT                                            *         
*          = 0 FOR MAXIMUM SCROLL                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SCRDOWN  LTR   R3,R1               R3 = SCROLL AMOUNT                           
         BNZ   SDOWN04             ZERO MEANS MAX. SCROLL                       
         TM    LSLTIND1,LSLTIEOL   TEST END-OF-LIST FOUND                       
         BO    SDOWN02                                                          
         LH    R1,LSLST#X          NO - BUILD ANOTHER PAGE FOR LIST             
         AH    R1,LSLINPAG                                                      
         BCTR  R1,0                ZERO BASE COUNT                              
         GOTOX ABLDLST,(R1)                                                     
         BL    EXITL                                                            
*                                                                               
SDOWN02  LH    RF,LSLST#X          1ST PAGE# = LAST LIST # - NO. LINES          
         LA    RF,1(RF)                                                         
         SH    RF,LSLINPAG                                                      
         STH   RF,LSPAG#1                                                       
         BM    *+14                ENSURE NOT GONE BEFORE FIRST LINE            
         CLC   LSPAG#1,LSLST#1                                                  
         BH    *+10                                                             
         MVC   LSPAG#1,LSLST#1                                                  
         B     SCRDOWNX                                                         
*                                                                               
SDOWN04  LH    RF,LSPAG#1          SET NEW PAGE START                           
         AR    RF,R3                                                            
         STH   RF,LSPAG#1                                                       
         LH    RE,LSLST#X                                                       
         TM    LSSTAT2,LSSADD      TEST ADDING TSAR RECORDS                     
         BZ    *+8                                                              
         LA    RE,1(RE)            YES - ALLOW 1 BLANK LINE AT END              
         CR    RF,RE               TEST ON CURRENT LIST                         
         BNH   SCRDOWNX                                                         
         TM    LSLTIND1,LSLTIEOL   TEST END-OF-LIST FOUND                       
         BO    SDOWN06                                                          
         LH    R1,LSPAG#1          NO - BUILD ANOTHER PAGE                      
         AH    R1,LSLINPAG                                                      
         BCTR  R1,0                ZERO BASE COUNT                              
         GOTOX ABLDLST,(R1)                                                     
         BL    EXITL                                                            
*                                                                               
         CLC   LSPAG#1,LSLST#X     TEST ON CURRENT LIST                         
         BNH   SCRDOWNX                                                         
*                                                                               
SDOWN06  MVC   LSPAG#1,LSLST#1     RE-DISPLAY PAGE 1                            
         B     SCRDOWNX                                                         
*                                                                               
SCRDOWNX GOTOX ASETPAG                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SCROLL UP                                                *         
*                                                                     *         
* NTRY: R1 = SCROLL AMOUNT                                            *         
*          = 0 FOR MAXIMUM SCROLL                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SCRUP    TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE LIST                        
         BZ    SCRUP02             NO                                           
         TM    GSSCRNUM,GSKIPAGE   SCROLLING A PAGE UP?                         
         BO    SCRUP06             YES                                          
*                                                                               
SCRUP02  LTR   R3,R1               TEST MAX SCROLL UP                           
         BZ    SCRUP04                                                          
         LH    RF,LSPAG#1          NO - TAKE AWAY SCROLL AMOUNT                 
         SR    RF,R3                                                            
         STH   RF,LSPAG#1                                                       
         BM    SCRUP04                                                          
         CLC   LSPAG#1,LSLST#1     TEST GONE BEFORE TOP OF THE LIST             
         BH    SCRUPX                                                           
*                                                                               
SCRUP04  MVC   LSPAG#1,LSLST#1                                                  
         OI    LSSCIND1,LSSCINUP                                                
         B     SCRUPX                                                           
*                                                                               
SCRUP06  L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         MVC   GCHALF,LSPAG#1      CURRENT START OF SCREEN                      
         XC    GCFULL2,GCFULL2                                                  
*                                                                               
SCRUP08  CLC   GCHALF,LSLST#1      STILL WITHIN BOUNDARIES OF LIST?             
         BH    SCRUP10             YES                                          
         MVC   LSPAG#1,LSLST#1                                                  
         OI    LSSCIND1,LSSCINUP                                                
         B     SCRUPX                                                           
*                                                                               
SCRUP10  LH    RF,GCHALF                                                        
         BCTR  RF,0                                                             
         STH   RF,GCHALF                                                        
         STCM  RF,3,TLNUM                                                       
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
         BE    *+6                                                              
         DC    H'0'                WHY NO RECORD?                               
*                                                                               
         L     RF,ATLST                                                         
         XR    RE,RE                                                            
         IC    RE,TLROWS-TLSTD(RF) LINES FOR THIS RECORD                        
         A     RE,GCFULL2          INCREMENT RUNNING TOTAL                      
         ST    RE,GCFULL2                                                       
         CH    RE,LSROWPAG         DOES THIS FIT STILL?                         
         BNH   SCRUP08             YES                                          
*                                                                               
         LH    RF,GCHALF                                                        
         LA    RF,1(RF)                                                         
         STH   RF,LSPAG#1          SET NEW FIRST PAGE                           
         B     SCRUPX                                                           
*                                                                               
SCRUPX   GOTOX ASETPAG                                                          
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SCROLL HORIZONTALLY                                      *         
*                                                                     *         
* NTRY: GSSCRNUM = SCROLL AMOUNT                                      *         
*       PFKEY PRESSED FOR SCROLLING                                   *         
***********************************************************************         
         SPACE 1                                                                
SCRHORZ  DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         CLI   GSFRP.FRPTYPE,FRPTLEFT                                           
         BE    SHORZ10             TEST SCROLLING RIGHT                         
*                                                                               
         TM    GSSCRNUM,GSKIMAXN   TEST MAXIMUM SCROLL                          
         BZ    SHORZ02                                                          
         LH    R1,LSVARNUM                                                      
         BCTR  R1,0                                                             
         B     SCRHORZL                                                         
*                                                                               
SHORZ02  LH    R1,LSVARLHS                                                      
         AH    R1,LCHORSCR                                                      
         CLM   R1,3,LSVARNUM       TEST GONE OVER END                           
         BL    *+6                                                              
         XR    R1,R1               YES - START AGAIN                            
         B     SCRHORZR                                                         
*                                                                               
SHORZ10  TM    GSSCRNUM,GSKIMAXN   TEST MAXIMUM SCROLL LEFT                     
         BZ    *+10                                                             
         XR    R1,R1                                                            
         B     SCRHORZR                                                         
         TM    GSSCRNUM,GSKIPAGE+GSKIHALF                                       
         BZ    SHORZ12                                                          
         LH    R1,LSVARRHS         ADHUST RHS IF PAGE/HALF                      
         SH    R1,LCHORSCR                                                      
         BNM   SCRHORZL                                                         
         XR    R1,R1                                                            
         B     SCRHORZR                                                         
SHORZ12  LH    R1,LSVARLHS         ADJUST LHS IF NUMERICAL INPUT                
         SH    R1,LCHORSCR                                                      
         BNM   *+6                                                              
         XR    R1,R1                                                            
         B     SCRHORZR                                                         
*                                                                               
SCRHORZL STH   R1,LSVARRHS                                                      
         GOTOX ABLDSCR,BSWLEFT                                                  
         B     EXITOK                                                           
*                                                                               
SCRHORZR STH   R1,LSVARLHS                                                      
         GOTOX ABLDSCR,BSWRIGHT                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT POSITION TO LIST LINE NUMBER                     *         
*                                                                     *         
* NTRY: P1 = POSITION                                                 *         
* EXIT: P2 = A(FIRST FIELD ON LINE)                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CURLIN   XR    R3,R3                                                            
         L     R3,0(R1)            R3=POSITION                                  
         LH    RF,LSLINAD          RF=DATA SCREEN ADDRESS OF 1ST LINE           
         SR    R3,RF                                                            
         BNP   CLINL               ABOVE THE LIST                               
*                                                                               
         XR    R2,R2                                                            
         LH    RF,LSCOLLIN                                                      
         TM    LSSTAT3,LS3RFIX     FIXED REPEATED ROWS                          
         BZ    *+8                                                              
         MH    RF,LSROWLIN                                                      
*                                                                               
         DR    R2,RF               R3=LIST LINE NUMBER                          
         CH    R3,LSNUMPAG                                                      
         BNL   CLINL               TOO FAR DOWN                                 
         MH    R3,LSLINLEN                                                      
         TM    LSSTAT3,LS3RFIX     FIXED REPEATED ROWS                          
         BZ    *+8                                                              
         MH    R3,LSROWLIN                                                      
*                                                                               
         AH    R3,LS1STLIN                                                      
         LA    R3,TWAD(R3)                                                      
*                                                                               
CLINOK   ST    R3,4(R1)            SAVE ADDRESS OF 1ST FIELD                    
         B     EXITOK                                                           
*                                                                               
CLINL    XC    0(4,R1),0(R1)       SET ADDRESS OF 1ST FIELD TO 0                
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SET PAGE INFO.                                           *         
*                                                                     *         
* NTRY: LSPAG#1 = NEW LINE NUMBER FOR TOP OF PAGE                     *         
* EXIT: LSPAG#X, LSNUMPAG ARE SET                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SETPAG   TM    LSSTAT3,LS3RVAR     VARIALBE NUMBER OF LINES ON SCREEN?          
         BZ    SPAG10                                                           
*                                                                               
         XC    LSLINPAG,LSLINPAG                                                
         XC    GCFULL2,GCFULL2                                                  
         L     R2,ATLST            SEE HOW MANY WE CAN GET ON SCREEN            
         USING TLSTD,R2                                                         
         MVC   TLNUM,LSPAG#1       FIRST FOR THIS LIST PAGE                     
         LA    R1,TSAGET                                                        
         B     *+8                                                              
*                                                                               
SPAG02   LA    R1,TSANXT                                                        
         GOTOX  ('TSARIO',AGROUTS),(R1)                                         
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,TLROWS         NUMBER OF ROWS REQUIRED                      
         BNZ   *+8                                                              
         LA    R0,1                                                             
*                                                                               
         A     R0,GCFULL2          CURRENT TOTAL                                
         CH    R0,LSROWPAG         WILL THIS RECORD FIT ON SCREEN               
         BH    SPAG04              NO                                           
*                                                                               
         ST    R0,GCFULL2          SET NEW ROW REQUIREMENT                      
         LH    RF,LSLINPAG         INCREMENT NUMBER OF ROWS THAT FIT            
         LA    RF,1(RF)                                                         
         STH   RF,LSLINPAG                                                      
*                                                                               
         CLC   TLNUM,LSLST#X       REACHED END OF LIST?                         
         BL    SPAG02              NO                                           
         DROP  R2                                                               
*                                                                               
SPAG04   OC    LSLINPAG,LSLINPAG   SEE HOW MANY RECORDS ON THIS SCRREN          
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES ON THIS TSAR RECORD           
*                                                                               
SPAG10   MVC   LSNUMPAG,LSLINPAG   SET MAX. NO OF LINES ON PAGE                 
*                                                                               
         LH    R1,LSPAG#1          SET LIST NO. OF END OF PAGE                  
         AH    R1,LSLINPAG                                                      
         BCTR  R1,0                                                             
         STH   R1,LSPAG#X                                                       
         CLC   LSPAG#X,LSLST#X     TEST WITHIN LIST                             
         BNH   SETPAGX                                                          
         GOTOX ABLDLST,(R1)        NO - BUILD MORE OF LIST                      
         BL    EXITL                                                            
*                                                                               
         TM    LSLTIND1,LSLTISOL   TEST ANYTHING IN LIST                        
         BO    SETPAG02                                                         
         XC    LSNUMPAG,LSNUMPAG   NO - RESET PAGE VALUES                       
         XC    LSPAG#1,LSPAG#1                                                  
         XC    LSPAG#X,LSPAG#X                                                  
         B     SETPAGX                                                          
*                                                                               
SETPAG02 CLC   LSPAG#X,LSLST#X     TEST WITHIN LIST NOW                         
         BNH   SETPAGX                                                          
         MVC   LSPAG#X,LSLST#X     NO - LIST FINISHES ON PAGE                   
         LH    RF,LSPAG#X                                                       
         SH    RF,LSPAG#1                                                       
         LA    RF,1(RF)                                                         
         STH   RF,LSNUMPAG         SET NUMBER OF LINES ON PAGE                  
*                                                                               
SETPAGX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIST LINES                                       *         
*                                                                     *         
* NTRY: LSLINE# = FIRST LINE TO BE DISPLAYED                          *         
* EXIT: LSLINE# + ON PAGE DISPLAYED                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DISPAG   TM    LSSTAT2,LSTSFULL    TEST TSAR BUFFER FULL                        
         BO    EXITL                                                            
*                                                                               
         OI    LSLTIND1,LSLTIDIS   SET LIST DISPLAY HAS NOW BEGUN               
         L     RF,ATLST                                                         
         USING TLSTD,RF                                                         
         XC    TLNUM,TLNUM         CLEAR TSAR BUFFER                            
         DROP  RF                                                               
*                                                                               
         TM    LSLTIND1,LSLTISOL   LIST HAS ITEMS?                              
         BZ    *+14                NO                                           
         CLC   LSLINE#,LSPAG#1     FIRST TIME FOR PAGE?                         
         BNE   DISPAG04            NO                                           
*                                                                               
         LH    R1,LS1STLIN         YES - CLEAR ALL LIST LINES                   
         A     R1,ATWA             R1=A(FIRST LIST LINE)                        
         LH    R0,LSLINPAG                                                      
         TM    LSSTAT3,LS3RVAR     VARIABLE ROWS?                               
         BZ    *+8                 NO                                           
         LH    R0,LSROWPAG                                                      
         TM    LSSTAT3,LS3RFIX     REPEATED ROWS?                               
         BZ    *+8                 NO                                           
         MH    R0,LSROWLIN                                                      
         L     RF,ACLRLINE                                                      
*                                                                               
DISPAG02 GOTOX (RF),(R1)                                                        
         AH    R1,LSLINLEN                                                      
         BCT   R0,DISPAG02                                                      
         TM    LSLTIND1,LSLTISOL   TEST ANY ITEMS IN LIST                       
         BZ    DISPAGX                                                          
*                                                                               
         XC    LSROWXTR,LSROWXTR   FOR ANY EXTRA ROWS USED                      
*                                                                               
DISPAG04 CLC   LSLINE#,LSPAG#X                                                  
         BH    DISPAGX                                                          
         GOTOX ADISLINE                                                         
         BL    EXITL               ERROR ON DISPLAY OF THIS LINE                
         LH    RE,LSLINE#                                                       
         LA    RE,1(RE)                                                         
         STH   RE,LSLINE#                                                       
         B     DISPAG04                                                         
*                                                                               
DISPAGX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ALL LIST LINES FOE A PAGE                       *         
*                                                                     *         
* NTRY: LSLINE# = FIRST LINE TO BE VALIDATED                          *         
*       SCROLLING/VALIDATION IF SELECT TO END-OF-LIST                 *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
VALPAG   L     RF,ATLST                                                         
         USING TLSTD,RF                                                         
         XC    TLNUM,TLNUM                                                      
         DROP  RF                                                               
*                                                                               
         TM    LSLTIND1,LSLTISOL   TEST ANY ITEMS IN LIST                       
         BZ    VALPAGX                                                          
*                                                                               
         XC    LSROWXTR,LSROWXTR   FOR ANY EXTRA ROWS USED                      
*                                                                               
VALPAG02 CLC   LSLINE#,LSPAG#X                                                  
         BH    VALPAG04                                                         
         GOTOX AVALLINE            VALIDATE THIS LINE                           
         BL    EXIT                ERROR                                        
         TM    LSLTIND1,LSLTISOL   TEST ANY ITEMS IN LIST                       
         BZ    VALPAGX                                                          
         TM    LSLNIND1,LSLNIDEL   TEST LINE DELETED                            
         BO    VALPAG02                                                         
         LH    RE,LSLINE#                                                       
         LA    RE,1(RE)                                                         
         STH   RE,LSLINE#                                                       
         B     VALPAG02                                                         
*                                                                               
VALPAG04 TM    LSSCIND1,LSSCIMEL   TEST SELECT TO END-OF-LIST                   
         BZ    VALPAGX                                                          
         TM    LSLTIND1,LSLTIEOL   TEST GOT TO END-OF-LIST                      
         BZ    *+14                                                             
         CLC   LSLST#X,LSPAG#X     YES - TEST LAST RECORD ON PAGE               
         BE    VALPAGX                                                          
         LH    R1,LSLINPAG         SCROLL DOWN 1 PAGE                           
         GOTOX ASCRDOWN,(R1)                                                    
         MVC   LSLINE#,LSPAG#1                                                  
         GOTOX ADISPAG             DISPLAY NEW PAGE                             
         MVC   LSLINE#,LSPAG#1                                                  
         B     VALPAG02            PROCESS NEW PAGE                             
*                                                                               
VALPAGX  TM    LSSCIND2,LSSCIDIS   TEST DISPLAY ONLY FOR LIST                   
         BO    EXITOK              YES                                          
         TM    LSSTAT2,LSSADD      TEST VALID TO ADD NEW LINES                  
         BZ    EXITOK                                                           
         GOTOX AVALNPAG                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REST OF PAGE FOR NEW LINES                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
VALNPAG  LH    R0,LSLINPAG                                                      
         SH    R0,LSNUMPAG         R0=NO. OF EXTRA LINES ON SCREEN              
         BZ    VNPAGX                                                           
         LH    R2,LSNUMPAG                                                      
         MH    R2,LSLINLEN                                                      
         AH    R2,LS1STLIN                                                      
*                                                                               
         A     R2,ATWA             R2=A(FIRST NEW LINE)                         
*                                                                               
VNPAG02  GOTOX AVALNLIN,(R2)       VALIDATE ALL NEW LINES ON SCREEN             
         BNE   EXITL                                                            
         NI    LSLNIND1,FF-LSLNIDEL                                             
         AH    R2,LSLINLEN                                                      
         BCT   R0,VNPAG02                                                       
*                                                                               
VNPAGX   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIST LINE                                        *         
*                                                                     *         
* NTRY: LSLINE# = LIST LINE NUMBER TO BE DISPLAYED                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DISLINE  LH    R2,LSLINE#          R2=LINE NUMBER                               
         SH    R2,LSPAG#1                                                       
         MH    R2,LSLINLEN                                                      
         TM    LSSTAT3,LS3RFIX     REPEATED SINGLE ROWS?                        
         BZ    *+8                                                              
         MH    R2,LSROWLIN         NUMBER OF ROWS REQUIRED FOR 1 LINE           
*                                                                               
         TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE ROWS?                       
         BZ    *+14                                                             
         LH    R0,LSROWXTR         'EXTRA' ROWS USED THUS FAR                   
         MH    R0,LSLINLEN                                                      
         AR    R2,R0                                                            
*                                                                               
         AH    R2,LS1STLIN                                                      
         STH   R2,LSCURLIN         SAVE DISPLACEMENT TO CURRENT LINE            
         A     R2,ATWA                                                          
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         GOTOX AGETITEM,LSLINE#    GET TSAR RECORD FOR THIS LIST LINE           
*                                                                               
         TM    LSLINEST,LSLINETO   SUB-TOTAL LINE?                              
         BZ    DISLN02             NO                                           
         CLI   LSSUBLEN,0          YES - PROTECT SUB-ACTION FIELD               
         BE    EXITOK                                                           
         OI    S.FHAT,FHATPR                                                    
         OI    S.FHOI,FHOITR                                                    
         B     EXITOK                                                           
*                                                                               
DISLN02  OC    LSSUBLEN,LSSUBLEN   SUB-ACTION FIELD?                            
         BZ    DISLN04                                                          
         NI    S.FHAT,FF-FHATPR    UNPROTECT SUB-ACTION FIELD                   
         OI    S.FHOI,FHOITR                                                    
         OI    S.FHII,FHIIVA                                                    
*                                                                               
DISLN04  GOTOX AGETREC,0           GET FILE RECORD                              
*                                                                               
         L     RF,LCAREC           A(FILE RECORD)                               
         TM    LSSTAT1,LSSTSAR     'VIRTUAL' LIST                               
         BZ    *+8                 NO                                           
         L     RF,ATLST            A(TSAR RECORD)                               
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DFIRST',DDIS),(RF),0           
         BL    EXITL               ERROR                                        
*                                                                               
         XC    LSROWREP,LSROWREP   CURRENT ROW REPEAT COUNTER                   
         LA    R0,1                                                             
         TM    LSSTAT3,LS3RFIX     FIXED REPEATED ROWS?                         
         BZ    *+12                NO                                           
         LH    R0,LSROWLIN         NUMBER OF ROWS PER RECORD                    
         B     DISLN06                                                          
*                                                                               
         TM    LSSTAT3,LS3RVAR     VARIABLE MULTIPLE ROWS?                      
         BZ    DISLN06             NO                                           
         L     RF,ATLST            COUNT SAVED ON TSAR RECORD                   
         ICM   R0,1,TLROWS-TLSTD(RF)                                            
         BNZ   *+6                                                              
         DC    H'0'                SET A REPEAT COUNT ON RECORD                 
*                                                                               
DISLN06  LA    R3,LSLIN            TABLE OF COLUMNS ON THE LINE                 
         USING LINTABD,R3                                                       
         LH    RE,LSROWREP         INCREMENT ROW REPEAT COUNT                   
         LA    RE,1(RE)                                                         
         STH   RE,LSROWREP         (THIS MAKES IT 1 BASED)                      
*                                                                               
         TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE BUILD?                      
         BZ    DISLN08             NO                                           
         L     RF,ATLST            FIRST REPEAT LINE?                           
         CLM   R0,1,TLROWS-TLSTD(RF)                                            
         BE    DISLN08             YES - HANDLED NORMALLY THEN                  
         OC    LSSUBLEN,LSSUBLEN   SUB-ACTION FIELD?                            
         BZ    DISLN08             NO                                           
         OI    S.FHAT,FHATPR       PROTECT SUB-ACTION FIELD                     
         OI    S.FHOI,FHOITR                                                    
         OI    S.FHII,FHIIVA                                                    
*                                                                               
DISLN08  CLI   LINTABD,LINTEOT     DISPLAYED ALL FIELDS?                        
         BE    DISLN14             YES                                          
         LH    RE,LINHDR                                                        
         LA    R4,S.FHD(RE)                                                     
         USING FHD,R4              R4=A(FIELD HEADER FOR OUTPUT)                
*                                                                               
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         OI    FHII,FHIIVA                                                      
         NI    FHAT,FF-FHATHI                                                   
*                                                                               
         TM    LSLINEST,LSLINEHI   SUB-TOTAL LINE?                              
         BZ    *+8                 NO                                           
         OI    FHAT,FHATHI         SUB-TOTALS ARE HIGHLIGHTED                   
*                                                                               
         TM    LININDS,LINIOPEN    ENSURE INPUT FIELD IS OPEN                   
         BZ    *+8                                                              
         NI    FHAT,FF-FHATPR                                                   
         MVC   FVIHDR,FHD          SAVE FIELD HEADER INFORMATION                
*                                                                               
         TM    LININDS,LINIOPEN    OPEN FIELD?                                  
         BZ    DISLN10             NO                                           
         GOTOX AGEN,RTPARM,ODATA,DDIS,FHD,LCAREC                                
         B     DISLN12                                                          
*                                                                               
DISLN10  GOTOX AGEN,RTPARM,ODATA,DNDIS,LINFLD#,LCAREC                           
*                                                                               
         LH    R1,LINFLD#          GET FDREL FOR CLOSED FIELD                   
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AFDRADDR                                                      
         L     R1,0(R1)                                                         
         USING FDRELD,R1                                                        
*                                                                               
         LH    RE,LINDSP           DISPLACEMENT TO COLUMN WITHIN FIELD          
         LA    RF,FHDA(RE)                                                      
         IC    RE,FDRLCLEN         COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      COPY FIELD INTO CLOSED FIELD                 
         DROP  R1                                                               
*                                                                               
DISLN12  LA    R3,LINTABL(R3)      NEXT FIELD                                   
         B     DISLN08             REPEAT FOR ALL FIELDS ON 1 LINE              
*                                                                               
DISLN14  AH    R2,LSLINLEN                                                      
         BCT   R0,DISLN06          REPEAT FOR ALL LINES FOR 1 RECORD            
         DROP  R3,R4                                                            
*                                                                               
         L     RF,LCAREC           A(FILE RECORD)                               
         TM    LSSTAT1,LSSTSAR     'VIRTUAL' LIST?                              
         BZ    *+8                 NO                                           
         L     RF,ATLST            A(TSAR RECORD)                               
         GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DLAST',DDIS),(RF),0            
         BL    EXITL                                                            
*                                                                               
         GOTOX APUTITEM,LSLINE#    WRITE BACK TSAR RECORD IF NECESSARY          
         BL    EXITL                                                            
*                                                                               
         TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE BUILD?                      
         BZ    EXITOK              NO                                           
         L     RF,ATLST                                                         
         XR    R0,R0                                                            
         ICM   R0,1,TLROWS-TLSTD(RF)                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RF,LSROWXTR         INCREMENT 'EXTRA' ROW COUNT                  
         AR    RF,R0                                                            
         BCTR  RF,0                FIRST ROW HANDLED NORMALLY                   
         STH   RF,LSROWXTR                                                      
         B     EXITOK                                                           
         DROP  S                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY TOTAL LINE                                       *         
*                                                                     *         
* NTRY: P1        = A(TOTAL LINE)                                     *         
*       P2 BYTE 0 = LINE NUMBER                                       *         
*               3 = VERB (DDISTOP / DDISBOT)                          *         
***********************************************************************         
         SPACE 1                                                                
DISTOT   DS    0H                                                               
         USING *,R7                                                             
         LM    R2,R3,0(R1)         R3=(LINE NUMBER, VERB)                       
         USING FHD,R2              R2=A(TOTAL FIELD)                            
         OI    FHOI,FHOITR                                                      
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         EX    RF,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
*                                                                               
         LR    RF,R3                                                            
         SRL   RF,24                                                            
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),0,('DFIRST',(R3)),        *        
               ((RF),FHD),0        ??                                           
*                                                                               
         LA    R4,LSLIN                                                         
         USING LINTABD,R4                                                       
DTOT02   CLI   LINTABD,LINTEOT                                                  
         BE    DTOT10                                                           
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,(R3),LINFLD#                                   
         LH    R1,LINFLD#                                                       
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AFDRADDR                                                      
         L     R1,0(R1)                                                         
         USING FDRELD,R1                                                        
         LH    RE,LINFCOL                                                       
         LA    RF,FHDA-1(RE)                                                    
         IC    RE,FDRLCLEN         COPY FIELD                                   
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD                                                   
         DROP  R1                                                               
*                                                                               
DTOT08   LA    R4,LINTABL(R4)                                                   
         B     DTOT02                                                           
         DROP  R4                                                               
*                                                                               
DTOT10   DS    0H                                                               
         LR    RF,R3                                                            
         SRL   RF,24                                                            
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),0,('DLAST',(R3)),         *        
               ((RF),FHD),0        ??                                           
*                                                                               
DISTOTX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE LIST LINE                                       *         
*                                                                     *         
* NTRY: LSLINE# = LIST LINE NUMBER TO BE VALIDATED                    *         
* EXIT: CC = ZERO, NO INPUT ON LINE                                   *         
*       CC = HIGH, RECORD UPDATED/RE-DISPLAYED                        *         
*       CC = LOW, ERROR                                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
VALLINE  XC    LSLNIND1,LSLNIND1   RESET LINE INDICATORS                        
         XC    LSLNIND2,LSLNIND2                                                
*                                                                               
         LH    R2,LSLINE#          CURRENT LINE NUMBER                          
         SH    R2,LSPAG#1          FIRST LINE ON PAGE                           
         MH    R2,LSLINLEN         LENGTH OF A LIST LINE                        
         TM    LSSTAT3,LS3RFIX     REPEATED SINGLE ROWS?                        
         BZ    *+8                                                              
         MH    R2,LSROWLIN         NUMBER OF ROWS REQUIRED FOR 1 LINE           
*                                                                               
         TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE ROWS?                       
         BZ    *+14                                                             
         LH    R0,LSROWXTR         'EXTRA' ROWS USED THUS FAR                   
         MH    R0,LSLINLEN                                                      
         AR    R2,R0                                                            
*                                                                               
         AH    R2,LS1STLIN                                                      
         STH   R2,LSCURLIN         SAVE DISPLACEMENT TO CURRENT LINE            
         A     R2,ATWA                                                          
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         GOTOX AGETITEM,LSLINE#    GET CURRENT TSAR RECORD                      
*                                                                               
         OC    LSSUBLEN,LSSUBLEN   SUB-ACTION FIELD?                            
         BE    VALIN02             NO                                           
         GOTOX AGEN,RTPARM,('GCBOVER',OSUBACT),SAVAL,S.FHD                      
         BL    VALINL              ERROR VALIDATING SUB-ACTION FIELD            
*                                                                               
         TM    LSLTIND1,LSLTISOL   TEST LIST STARTED                            
         BZ    EXITOK              NO                                           
         TM    LSLNIND1,LSLNIDEL   TEST DELETE LINE REQUESTED                   
         BO    VALIN24             YES                                          
*                                                                               
VALIN02  XC    LSROWREP,LSROWREP   CURRENT ROW INDICATOR                        
         LA    R0,1                                                             
         TM    LSSTAT3,LS3RFIX     FIXED MULTIPLE ROWS?                         
         BZ    *+12                NO                                           
         LH    R0,LSROWLIN                                                      
         B     VALIN04                                                          
*                                                                               
         TM    LSSTAT3,LS3RVAR     VARIABLE MULTIPLE ROWS?                      
         BZ    VALIN04             NO                                           
         L     RF,ATLST                                                         
         ICM   R0,1,TLROWS-TLSTD(RF)                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALIN04  STH   R0,LCHALF           STORE REPEAT COUNT FOR LATER                 
*                                                                               
         LA    R3,LSLIN            COLUMNS ON THIS LINE                         
         USING LINTABD,R3                                                       
*                                                                               
         LH    RE,LSROWREP         INCREMENT ROW REPEAT COUNT                   
         LA    RE,1(RE)                                                         
         STH   RE,LSROWREP         (THIS MAKES IT 1 BASED)                      
*                                                                               
VALIN06  CLI   LINTABD,LINTEOT     VALIDATED ALL COLUMNS?                       
         BE    VALIN14             YES                                          
         TM    LININDS,LINIOPEN    EDITABLE COLUMN?                             
         BZ    VALIN12             NO                                           
         TM    LSSCIND2,LSSCIDIS   TEST DISPLAY ONLY                            
         BO    VALIN12             YES                                          
*                                                                               
         LH    RE,LINHDR           DISP TO HEADER ON LINE                       
         LA    R4,S.FHD(RE)                                                     
         USING FHD,R4                                                           
*                                                                               
         TM    FHAT,FHATPR         MAKE SURE 'OPEN' FIELD IS OPEN               
         BO    VALIN12             NO - IGNORE IT THEN                          
*                                                                               
         LH    RF,LINFLD#          FIELD IDENTIFIER                             
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         A     RF,AFDRADDR         INDEX INTO FIELD ELEMENT ARRAY               
         L     R5,0(RF)                                                         
         USING FDRELD,R5                                                        
*                                                                               
         TM    FHII,FHIITH         FIELD INPUT THIS TIME?                       
         BO    VALIN08             YES                                          
         TM    FHII,FHIIVA         FIELD INPUT?                                 
         BZ    VALIN08             YES                                          
*                                                                               
         GOTOX AFNDMIF,RTPARM,FHD,FDRELD  LOOK FOR MULTI-INPUT FIELD            
         B     VALIN12                                                          
*                                                                               
VALIN08  GOTOX ('FLDVAL',AGROUTS),FHD                                           
         XR    R0,R0                                                            
         CLI   FVILEN,0            FIELD HAS DATA TO VALIDATE?                  
         BE    VALIN10             NO                                           
         TM    FDRLIND1,FDRLIEOP+FDRLIEOL                                       
         BZ    VALIN10             NOT VALID DO MULTI-INPUT SELECTIONS          
*                                                                               
         GOTOX ATSTMIF,FDRELD      CHECK IF REQUESTED MULTI-INPUT FIELD         
         BL    VALIN10             NO MULTI-INPUT                               
         BH    VALIN12             PROCESS NEXT                                 
         LA    R0,LCMIFELD         R0=A(NEW MIFELD)                             
*                                                                               
VALIN10  GOTOX AVALFLD,(R0)        VALIDATE FIELD                               
         BNE   VALINL                                                           
         TM    LSLNIND1,LSLNIDEL   TEST DELETE LINE                             
         BO    VALIN24             YES                                          
         LTR   R0,R0               ADDING MIFEL?                                
         BZ    VALIN12             NO                                           
         GOTOX AADDMIF                                                          
         BNE   VALINL                                                           
*                                                                               
VALIN12  LA    R3,LINTABL(R3)      NEXT COLUMN IN LINE                          
         B     VALIN06                                                          
         DROP  R3                                                               
*                                                                               
VALIN14  LH    R0,LCHALF           NEXT LINE FOR RECORD                         
         AH    R2,LSLINLEN                                                      
         BCT   R0,VALIN04                                                       
*                                                                               
         TM    LSLNIND1,LSLNIUPD   UPDATE RECORD REQUIRED                       
         BZ    VALIN18             NO                                           
*                                                                               
         L     RF,LCAREC           A(FILE RECORD)                               
         TM    LSSTAT1,(LSSMAIN+LSSTSAR)                                        
         BZ    *+8                                                              
         L     RF,ATLST            A(TSAR RECORD)                               
         GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DLAST',DVAL),(RF),0            
         IPM   RF                                                               
*                                                                               
         TM    LSSTAT1,(LSSMAIN+LSSTSAR)                                        
         BZ    *+14                ONLY REAL RECORDS WRITTEN TO FILE            
         SPM   RF                                                               
         BL    EXITL               ERROR ON THE DLAST CALL                      
         B     VALIN20                                                          
*                                                                               
VALIN16  SPM   RF                                                               
         BL    VALINL              ERROR ON THE DLAST CALL                      
*                                                                               
         USING TLSTD,R3                                                         
         L     R3,ATLST                                                         
         LA    R0,IRECWRT          SET TO WRITE RECORD TO FILE                  
         TM    TLRINFO1,TLR1XFIL   TEST RECORD ALREADY ON FILE                  
         BZ    *+12                YES                                          
         LA    R0,IRECADD          SET TO ADD RECORD TO FILE                    
         B     VALIN17                                                          
*                                                                               
         OC    TLRDA(2),TLRDA      IS THIS A GETFLD RECORD?                     
         BNZ   *+12                YES - DO NOTHING THEN                        
         CLI   GSSYS,QSCON                                                      
         BE    VALIN20                                                          
*                                                                               
VALIN17  MVC   LCBYTE,GSFRR.FRRINDS1  SAVE RECORD INDICATORS                    
         NI    GSFRR.FRRINDS1,FF-(FRR1UPDT)  ENSURE UPDATIVE FLAG OFF           
*                                                                               
         GOTOX APUTITEM            SAVE TSAR RECORD                             
         GOTOX AGEN,RTPARM,OIO,(R0)                                             
*                                                                               
         NI    TLRINFO1,FF-TLR1XFIL   SET RECORD ON FILE (DEFINATELY)           
         MVC   GSFRR.FRRINDS1,LCBYTE  RESTORE RECORD INDICATORS                 
         GOTOX AGETITEM,LSLINE#       GET TSAR RECORD BACK                      
         B     VALIN20                                                          
*                                                                               
VALIN18  TM    LSLNIND1,LSLNIINP+LSLNIRED                                       
         BNZ   VALIN20             LINE HAS CHANGED                             
*                                                                               
         TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE BUILD?                      
         BZ    EXITOK              NO                                           
         L     RF,ATLST                                                         
         XR    R0,R0                                                            
         ICM   R0,1,TLROWS-TLSTD(RF)                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RF,LSROWXTR         INCREMENT 'EXTRA' ROW COUNT                  
         AR    RF,R0                                                            
         BCTR  RF,0                FIRST ROW HANDLED NORMALLY                   
         STH   RF,LSROWXTR                                                      
         B     EXITOK                                                           
*                                                                               
VALIN20  TM    LSLNIND1,LSLNIDEL   LINE DELETE REQUESTED?                       
         BO    VALIN24             YES                                          
         GOTOX APUTITEM            SAVE TSAR RECORD BACK                        
*                                                                               
VALIN22  GOTOX ADISLINE            REDISPLAY LINE - FIXES UP LSROWXTR           
         BL    VALINL                                                           
         B     EXITH                                                            
*                                                                               
VALIN24  GOTOX AREMLINE            DELETE LINE & RESTORE SCREEN                 
         B     EXITH                                                            
*                                                                               
VALINL   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
         DROP  S,R3,R4,R5                                                       
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE NEW LIST LINE                                   *         
*                                                                     *         
* NTRY: LSLINE# = LIST LINE NUMBER TO BE VALIDATED                    *         
* EXIT: CC = ZERO, NO INPUT ON LINE                                   *         
*       CC = HIGH, RECORD UPDATED/RE-DISPLAYED                        *         
*       CC = LOW, ERROR                                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
*                                                                               
VALNLIN  MVI   LSLNIND1,0                                                       
         MVI   LSLNIND2,0                                                       
         LR    R2,R1                                                            
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         GOTOX ATSTLINE,S.FHD      ANY INPUT TO LINE?                           
         BNE   EXITOK              NO                                           
*                                                                               
         S     R2,ATWA                                                          
         STH   R2,LSCURLIN         SAVE DISPLACEMENT TO CURRENT LINE            
         A     R2,ATWA                                                          
*                                                                               
         GOTOX ANEWITEM,LSLINE#    SET UP NEW ITEM                              
         CLI   LSSUBLEN,0          SUB-ACTION FIELD?                            
         BE    VNL02               NO                                           
         TM    LSSTAT3,LS3VSAD     WANT TO ALWAYS VALIDATE SUB-ACT FLD?         
         BO    *+12                YUP                                          
         TM    LSLTIND1,LSLTISOL   TEST LIST STARTED                            
         BZ    VNL02               NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',OSUBACT),SAVAL,S.FHD                      
         BL    VNLL                ERROR VALIDATING SUB-ACTION FIELD            
         TM    LSLNIND1,LSLNIDEL   TEST DELETE LINE REQUESTED                   
         BO    EXITOK              YES                                          
*                                                                               
VNL02    XC    LSROWREP,LSROWREP                                                
         LA    R0,1                                                             
*                                                                               
VNL03    STH   R0,LCHALF                                                        
         LA    R3,LSLIN                                                         
         USING LINTABD,R3                                                       
*                                                                               
         LH    R4,LSROWREP                                                      
         LA    R4,1(R4)                                                         
         STH   R4,LSROWREP                                                      
*                                                                               
VNL04    CLI   LINTABD,LINTEOT     VALIDATED ALL COLUMNS                        
         BE    VNL08                                                            
         TM    LININDS,LINIOPEN    EDITABLE COLUMN?                             
         BZ    VNL06               NO                                           
*                                                                               
         LH    R4,LINHDR           DISP TO HEADER ON LINE                       
         LA    R4,S.FHD(R4)                                                     
         LH    RF,LSROWREP         INDEX IN FOR MULTIPLE ROWS                   
         BCTR  RF,0                                                             
         MH    RF,LSLINLEN                                                      
         LA    R4,0(R4,RF)                                                      
         USING FHD,R4              R4=A(FIELD HEADER)                           
         TM    FHAT,FHATPR         MAKE SURE 'OPEN' FIELD IS OPEN               
         BO    VNL06                                                            
*                                                                               
         LH    RF,LINFLD#          GET FIELD ELEMENT FOR THIS FIELD             
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         A     RF,AFDRADDR                                                      
         L     R5,0(RF)                                                         
         USING FDRELD,R5                                                        
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         GOTOX AVALFLD,0                                                        
         BNE   VNLL                                                             
*                                                                               
         TM    LSLNIND1,LSLNIDEL   DELETE LINE REQUESTED                        
         BO    VNL14               YES                                          
*                                                                               
VNL06    LA    R3,LINTABL(R3)                                                   
         B     VNL04                                                            
         DROP  R3,R4,R5                                                         
*                                                                               
VNL08    LH    R0,LCHALF                                                        
         BCT   R0,VNL03                                                         
*                                                                               
         TM    LSLNIND1,LSLNIUPD   UPDATE RECORD?                               
         BZ    VNL14               NO                                           
         TM    LSSTAT1,LSSTSAR+LSSMAIN                                          
         BZ    VNL10                                                            
         GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DLAST',DVAL),ATLST,0           
         BL    EXITL                                                            
         TM    LSLNIND1,LSLNIDEL   TEST DELETE LINE REQUESTED                   
         BO    VNL14               YES - CLEAR LINE                             
         B     VNL12                                                            
*                                                                               
VNL10    GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DLAST',DVAL),LCAREC,0          
         BL    EXITL                                                            
         TM    LSLNIND1,LSLNIDEL   TEST DELETE LINE REQUESTED                   
         BO    EXITOK              YES                                          
         GOTOX AGEN,RTPARM,OIO,IRECADD  ??                                      
*                                                                               
VNL12    L     R3,ATLST                                                         
         USING TLSTD,R3            R3=A(TSAR RECORD)                            
         L     R5,ATSABLK                                                       
         USING TSARD,R5            R5=A(TSARIO BLOCK)                           
*                                                                               
         MVC   LCWORK1(L'TLKSRT),TLKSRT                                         
         MVI   TLKSRT,FF           ADD KEY TO END OF LIST                       
         MVC   TLKSRT+1(L'TLKSRT-1),TLKSRT                                      
         GOTOX ALST,RTPARM,OLIST,LTSARADD                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$TOMNY)                                           
         B     EXITL                                                            
*                                                                               
         MVC   TLKSRT,LCWORK1      RESTORE ORIGINAL KEY                         
         OI    TSINDS,TSIKEYUP                                                  
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         NI    TSINDS,FF-TSIKEYUP                                               
         DROP  R3,R5                                                            
*                                                                               
         OI    LSLTIND1,LSLTISHU   SET SHUFFLE REQUIRED                         
         GOTOX ACLRLINE,S.FHD                                                   
         GOTOX ASETPAG                                                          
         MVC   LSLINE#,LSLST#X                                                  
         GOTOX ADISLINE                                                         
         B     EXITOK                                                           
*                                                                               
VNL14    GOTOX ACLRLINE,S.FHD                                                   
         B     EXITOK                                                           
*                                                                               
VNLL     CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               CC=LOW - ERROR                               
         DROP  S                                                                
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO FIND MULTI-INPUT FOR FIELD                               *         
*                                                                     *         
* NTRY: P1 = A(FIELD HEADER)                                          *         
*       P2 = A(FIELD RECORD ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
FNDMIF   LM    R2,R3,0(R1)                                                      
         USING FHD,R2                                                           
         USING FDRELD,R3                                                        
         LA    R4,LCMIFS                                                        
         USING MIFELD,R4                                                        
FMIF02   CLI   MIFEL,0             TEST END-OF-LIST                             
         BE    FNDMIFN                                                          
         CLC   MIFFLD#,FDRNUM      MATCH ON FIELD NUMBER                        
         BNE   FMIF08                                                           
         CLC   LSLINE#,MIF#1       MATCH ON LINE NUMBER                         
         BL    FMIF08                                                           
         CLC   LSLINE#,MIF#X                                                    
         BH    FMIF08                                                           
*                                                                               
         IC    RE,FHLN             SAVE FIELD                                   
         EX    RE,*+4                                                           
         MVC   LCSAVFLD(0),FHD                                                  
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),LCSPACES                                                 
         NI    FHII,FF-FHIIVA                                                   
         XR    RF,RF                                                            
         ICM   RF,1,MIFFLDLN                                                    
         BCTR  RF,0                                                             
         BZ    FMIF04                                                           
         XR    RE,RE                                                            
         IC    RE,MIFQUALN                                                      
         LA    RE,MIFQUA(RE)                                                    
         EX    RF,*+4                                                           
         MVC   FHDA(0),0(RE)                                                    
FMIF04   GOTOX ('FLDVAL',AGROUTS),FHD                                           
         GOTOX AVALFLD,MIFELD      VALIDATE INPUT                               
         BE    FNDMIFY                                                          
         IC    RE,FHLN             INVALID - RESTORE INPUT AS WAS               
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FHD(0),LCSAVFLD                                                  
*                                                                               
FMIF08   XR    RF,RF               BUMP TO NEXT MULTI-INPUT FIELD               
         IC    RF,MIFLN                                                         
         BXH   R4,RF,FMIF02                                                     
*                                                                               
FNDMIFY  B     EXITOK                                                           
FNDMIFN  B     EXITL                                                            
         DROP  R2,R3,R4                                                         
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO TEST INPUT FOR MULTI-INPUT                               *         
*                                                                     *         
* NTRY: R1 = A(FIELD RECORD ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TSTMIF   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         LR    R3,R1                                                            
         USING FDRELD,R3                                                        
         L     R2,FVADDR                                                        
         USING FHD,R2                                                           
*                                                                               
* ??     CLI   FVIFLD,C'*'         TEST KEEP FIELD AS IS                        
*        BNE   TMIF02                                                           
*        OI    LSLNIND1,LSLNIRED   YES - SET RE-DISPLAY RECORD                  
*        B     TSTMIFH                                                          
*                                                                               
         USING MIFELD,LCMIFELD                                                  
TMIF02   XR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         LA    RE,FHDA-1(RE)                                                    
         TM    FDRLIND1,FDRLIEOP   TEST SELECTION TO END-OF-PAGE                
         BZ    TMIF04                                                           
         CLI   0(RE),C'+'                                                       
         BNE   TMIF04                                                           
         MVC   MIF#X,LSPAG#X                                                    
         B     TMIF06                                                           
TMIF04   TM    FDRLIND1,FDRLIEOL   TEST SELECTION TO END-OF-LIST                
         BZ    TSTMIFL                                                          
         CLI   0(RE),C'&&'                                                      
         BNE   TSTMIFL                                                          
         MVC   MIF#X,BCEFFS                                                     
*                                                                               
TMIF06   MVI   0(RE),C' '          REMOVE +/& FROM INPUT FIELD                  
         MVC   MIFEL,MIFELQ                                                     
         MVC   MIFFLD#,FDRNUM                                                   
         MVC   MIF#1,LSLINE#                                                    
*                                                                               
TSTMIFY  B     EXITOK              CC=EQUAL - NEW MIFEL                         
*                                                                               
TSTMIFL  B     EXITL               CC=LOW - NO MIFEL                            
*                                                                               
TSTMIFH  B     EXITH               CC=HIGH - PROCESS NEXT                       
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD NEW MIFELD                                           *         
***********************************************************************         
         SPACE 1                                                                
ADDMIF   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
N        USING MIFELD,LCMIFELD                                                  
         LA    R3,LCMIFS                                                        
         USING MIFELD,R3                                                        
         XR    RF,RF                                                            
AMIF02   CLI   MIFELD,0            FIND ANY EXISTING MIFELD FOR FIELD           
         BE    AMIF10                                                           
         CLC   MIFFLD#,N.MIFFLD#                                                
         BNE   AMIF08                                                           
         ICM   RF,1,MIFQUALN       COMPARE QUALIFIER                            
         BZ    AMIF04                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   AMIF08                                                           
AMIF04   MVI   MIFELD,FF           REMOVE THIS ELEMENT                          
         GOTOX VHELLO,RTPARM,(C'D',CORETAB),('FF',LCMIF),0                      
         B     AMIF10                                                           
*                                                                               
AMIF08   IC    RF,MIFLN                                                         
         BXH   R3,RF,AMIF02                                                     
         DROP  R3                                                               
*                                                                               
AMIF10   XR    RF,RF                                                            
         IC    RF,N.MIFQUALN       RF=QUALIFIER LENGTH                          
         LA    R1,N.MIFQUA(RF)                                                  
         MVC   N.MIFFLDLN,FVILEN                                                
         XR    RE,RE                                                            
         ICM   RE,1,N.MIFFLDLN     RE=INPUT FIELD LENGTH                        
         BZ    AMIF12                                                           
         EX    RE,*+4                                                           
         MVC   0(0,R1),FVIFLD                                                   
AMIF12   LA    RE,MIFLNQ(RF,RE)    SET ELEMENT LENGTH                           
         STC   RE,N.MIFLN                                                       
         GOTOX VHELLO,RTPARM,(C'P',CORETAB),LCMIF,N.MIFELD                      
         CLC   LCMIFL,=AL2(L'LCMIF)                                             
         BL    ADDMIFY                                                          
         MVC   FVMSGNO,=AL2(FVFTOOM)   TOO MANY MULTIS                          
*                                                                               
ADDMIFN  B     EXITL                                                            
*                                                                               
ADDMIFY  B     EXITOK                                                           
         DROP  N                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE FIELD                                           *         
*                                                                     *         
* NTRY: R1 = A(MIFELD ENTRY) OR 0                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
VALFLD   OI    LSSCIND1,LSSCIINP   SET SCREEN HAS INPUT                         
         TM    LSLNIND1,LSLNIINP   LINE PREVIOUSLY INPUT?                       
         BNZ   VFLD02              YES                                          
*                                                                               
         TM    LSSTAT1,LSSTSAR     RECORD EXISTS ON FILE?                       
         BO    VFLD02              NO                                           
         XR    R1,R1                                                            
         TM    LSSTAT1,LSSMAIN     MAINTENANCE LISTS DON'T UPDATE FILE          
         BO    *+8                                                              
         L     R1,=AL4(XOLOCK)                                                  
         GOTOX AGETREC                                                          
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DFIRST',DVAL),LCAREC,0         
*                                                                               
VFLD02   GOTOX AGEN,RTPARM,ODATA,DVAL,FVADDR,LCAREC                             
         BNE   VALFLDN                                                          
*                                                                               
         OI    LSLNIND1,LSLNIUPD ?? WRONG BUT I CAN'T BE BOTHERED ??            
*                                                                               
VALFLDY  OI    LSLNIND1,LSLNIINP   SET INPUT                                    
         OI    GCINDS2,GCIANYCH                                                 
         B     EXITOK                                                           
*                                                                               
VALFLDN  B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REMOVE CURRENT LINE FROM LIST                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
REMLINE  LA    R0,SVLST                                                         
         LA    R1,L'SVLST                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SAVED LIST LINE                        
*                                                                               
         LA    R3,1                                                             
         TM    LSSTAT3,LS3RFIX     FIXED NUMBER OF REPEAT LINES?                
         BZ    *+8                 NO                                           
         LH    R3,LSROWLIN                                                      
         TM    LSSTAT3,LS3RVAR     VARIABLE NUMBER OF REPEAT LINES?             
         BZ    *+12                                                             
         L     RF,ATLST            GET NUMBER OF ROWS                           
         ICM   R3,1,TLROWS-TLSTD(RF)                                            
         MH    R3,LSLINLEN                                                      
*                                                                               
         GOTOX ALST,RTPARM,OLIST,LTSARDEL                                       
         GOTOX ASETPAG             RESET PAGE VALUES                            
*                                                                               
         LH    R2,LSCURLIN         SCROLL LIST LINES UP 1                       
         A     R2,ATWA                                                          
T        USING FHD,R2              R2=A(COPY TO LINE)                           
         AR    R3,R2                                                            
F        USING FHD,R3              R3=A(COPY FROM LINE)                         
*                                                                               
         LH    RF,LSBOTDSP                                                      
         SH    RF,LSLINLEN                                                      
         A     RF,ATWA                                                          
         BCTR  RF,0                RF=A(END OF COPY TO AREA)                    
         CR    R2,RF                                                            
         BH    RLINE04                                                          
         XR    RE,RE                                                            
*                                                                               
RLINE02  IC    RE,F.FHLN                                                        
         LH    R0,T.FHAD           SAVE/RESTORE SCREEN ADDRESS                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   T.FHD(0),F.FHD                                                   
         STH   R0,T.FHAD                                                        
         OI    T.FHOI,FHOITR                                                    
         LA    RE,1(RE)                                                         
         AR    R2,RE               ??                                           
         BXLE  R3,RE,RLINE02       ??                                           
         DROP  F,T                                                              
*                                                                               
RLINE04  TM    LSLTIND1,LSLTISOL   TEST WAS ONLY RECORD IN LIST                 
         BZ    EXITL                                                            
         CLC   LSNUMPAG,LSLINPAG   TEST LIST PAGE FULL                          
         BE    RLINE06                                                          
         LH    R1,LSNUMPAG                                                      
         MH    R1,LSLINLEN                                                      
         AH    R1,LS1STLIN                                                      
         A     R1,ATWA                                                          
         GOTOX ACLRLINE,(R1)                                                    
         OC    LSNUMPAG,LSNUMPAG   TEST PAGE NOW EMPTY                          
         BNZ   REMLINEX                                                         
         MVC   LSPAG#1,LSLST#X     DISPLAY LAST LINE                            
         GOTOX ASETPAG                                                          
         MVC   LSLINE#,LSPAG#1                                                  
*                                                                               
RLINE06  CLI   LSSUBLEN,0          SUB-ACTION FIELD TO CLEAR?                   
         BE    RLINE08             NO                                           
*                                                                               
         LH    R1,LSLINPAG                                                      
         BCTR  R1,0                                                             
         MH    R1,LSLINLEN                                                      
         AH    R1,LS1STLIN                                                      
         A     R1,ATWA                                                          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,LSSUBLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         XC    FHDA-FHD(0,R1),FHDA-FHD(R1)                                      
*                                                                               
RLINE08  LH    R0,LSLINE#                                                       
         MVC   LSLINE#,LSPAG#X                                                  
         GOTOX ADISLINE                                                         
         STH   R0,LSLINE#                                                       
*                                                                               
REMLINEX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAR LIST LINE                                          *         
*                                                                     *         
* NTRY: R1 = A(LIST LINE)                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
         USING FHD,R1                                                           
CLRLINE  LA    RF,FHD                                                           
         AH    RF,LSLINLEN                                                      
         BCTR  RF,0                SET RF TO END OF LINE - 1                    
         XR    RE,RE                                                            
*                                                                               
CLINE02  ICM   RE,1,FHLN           SET INDEX IN RE                              
         BZ    CLINE04                                                          
         TM    LSSTAT2,LSSADD      ALLOWED TO ADD ITEMS TO LIST?                
         BO    *+8                 YES                                          
         OI    FHAT,FHATPR         NO - PROTECT ALL FIELDS                      
         OI    FHII,FHIIVA         SET FIELD VALIDATED                          
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         LR    R2,RE                                                            
         SH    R2,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BO    *+8                                                              
         LA    R2,FHDAD(R2)                                                     
         EX    R2,*+4                                                           
         MVC   FHDA(0),LCSPACES    SPACE FILL TEXT PORTION                      
         BXLE  R1,RE,CLINE02       REPEAT FOR ALL FIELDS                        
*                                                                               
CLINE04  B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO TEST LINE FOR ANY INPUT                                  *         
*                                                                     *         
* NTRY: R1 = A(LIST LINE)                                             *         
* EXIT: CC = LOW - NO INPUT TO LINE                                   *         
*       CC = EQ  - INPUT TO LINE                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
         USING FHD,R1                                                           
TSTLINE  LA    RF,FHD                                                           
         AH    RF,LSLINLEN                                                      
         BCTR  RF,0                RF = A(END OF LINE) - 1                      
         XR    RE,RE                                                            
*                                                                               
TLINE02  IC    RE,FHLN                                                          
         TM    FHAT,FHATPR         IGNORE PROTECTED FIELDS                      
         BO    *+12                                                             
         TM    FHII,FHIIVA         FIELD HAS INPUT?                             
         BZ    EXITOK              YES                                          
*                                                                               
         BXLE  R1,RE,TLINE02                                                    
         B     EXITL               NO INPUT TO THIS LINE                        
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO CALCULATE HORIZONTAL SCROLL AMOUNT                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SCRHAMT  LH    R1,LSVARPAG         PAGE SCROLL                                  
         TM    GSSCRNUM,GSKIPAGE+GSKIMAXN                                       
         BNZ   SCRHAMTX                                                         
         TM    GSSCRNUM,GSKIHALF                                                
         BO    *+12                                                             
         IC    R1,GSSCRNUM         NUMERICAL SCROLL                             
         B     SCRHAMTX                                                         
         SRA   R1,1                HALF PAGE SCROLL                             
         BNZ   *+8                                                              
         LA    R1,1                                                             
SCRHAMTX STH   R1,LCHORSCR         SAVE SCROLL AMOUNT                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CALCULATE VERTICAL SCROLL AMOUNT                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SCRVAMT  LH    R1,LSLINPAG         PAGE SCROLL                                  
         TM    GSSCRNUM,GSKIPAGE+GSKIMAXN                                       
         BNZ   SCRVAMTX                                                         
*                                                                               
         TM    GSSCRNUM,GSKIHALF                                                
         BO    *+12                                                             
         IC    R1,GSSCRNUM         NUMERICAL SCROLL                             
         B     SCRVAMTX                                                         
*                                                                               
         CLI   GSFRP.FRPTYPE,FRPTUP                                             
         BE    *+8                                                              
         LH    R1,LSNUMPAG                                                      
         SRA   R1,1                HALF PAGE SCROLL                             
         BNZ   *+8                                                              
         LA    R1,1                                                             
*                                                                               
SCRVAMTX STH   R1,LCVERSCR         SAVE SCROLL AMOUNT                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET RECORD                                               *         
*                                                                     *         
* NTRY: R1 = IOLOCK TO READ FOR UPDATE                                *         
*       R1 = 0 TO JUST READ                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
GETREC   L     R2,ATLST            R2=A(CURRENT TSAR RECORD)                    
         USING TLSTD,R2                                                         
         TM    LSSTAT1,LSSTSAR     RECORD EXISTS ON FILE?                       
         BO    EXITOK              NO                                           
*                                                                               
         LR    R3,R1               R3=IOLOCK/0                                  
         CLM   R3,7,=XL3'FFFFFF'   SPECIAL EXTERNAL CALL                        
         BNE   *+6                                                              
         XR    R3,R3                                                            
         MVC   GSRECKEY,TLRKEY     SET UP DIRECTORY INFORMATION                 
         MVC   GSRECSTA,TLRSTA                                                  
         MVC   GSRECMSK,TLRMSK                                                  
         MVC   GSRECDA,TLRDA                                                    
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECRD,(R3),TLRDA,LCEQUREC                       
         BL    EXITL               ERROR READING FROM DISK ADDRESS              
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET TSAR RECORD FROM LIST                                *         
*                                                                     *         
* NTRY: R1=TSAR RECORD NUMBER REQUIRED                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
GETITEM  L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         CLM   R1,7,=XL3'FFFFFF'   SPECIAL EXTERNAL CALL                        
         BNE   *+8                                                              
         LA    R1,LSLINE#                                                       
         CLC   TLNUM,0(R1)         TEST ALREADY HAVE IT                         
         BE    EXIT                                                             
*                                                                               
         LA    RE,TLREC            CLEAR TSAR RECORD INPUT BUFFER               
         LA    RF,L'TLREC                                                       
         XR    R3,R3               ANY ODD REGISTER                             
         MVCL  RE,R2                                                            
*                                                                               
         MVC   TLNUM,0(R1)                                                      
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
         GOTOX ASAVITEM                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TSAR RECORD FOR NEW ITEM TO ADD INTO LIST     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
NEWITEM  L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         LA    R0,TLSTD            CLEAR TSAR RECORD BUFFER                     
         LA    R1,L'TLST                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LH    RF,LSLST#X          CURRENT HIGH LIST NUMBER                     
         LA    RF,1(RF)            INCREMENT BY 1                               
         STCM  RF,3,TLNUM          SAVE NUMBER FOR NEW RECORD                   
         MVC   TLRLEN,=AL2(TLMINLNQ)  SET DEFAULT LENGTH                        
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',OLIST),LNEWITEM,TLSTD                     
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PUT ITEM BACK TO LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
PUTITEM  L     R2,ATLST                                                         
         USING TLSTD,R2            CURRENT COPY OF LIST RECORD                  
S        USING TLSTD,SVLST         SAVED COPY OF LIST RECORD                    
*                                                                               
         LA    RE,TLSTD            TEST CHANGE IN TSAR RECORD                   
         LA    R0,S.TLSTD                                                       
         XR    RF,RF                                                            
         ICM   RF,3,TLRLEN                                                      
         LA    RF,L'TLNUM(RF)                                                   
         LR    R1,RF                                                            
         CLCL  RE,R0                                                            
         BE    EXIT                TSAR RECORD UNCHANGED                        
*                                                                               
         OI    LSSCIND2,LSSCITSC   SET TSAR RECORD CHANGED FLAG                 
*                                                                               
         CLC   TLKEY,S.TLKEY       CHANGE TO KEY?                               
         BNE   PITM02              YES                                          
*                                                                               
         GOTOX ('TSARIO',AGROUTS),TSAPUT  PUT IT BACK                           
         GOTOX ASAVITEM            SAVE A COPY OF NEW RECORD                    
         B     EXITOK                                                           
*                                                                               
         USING TSARD,R3                                                         
PITM02   L     R3,ATSABLK                                                       
         OI    TSINDS,TSIKEYUP     TELL TSAR TO PUT BACK REGARDLESS             
         OI    TLSTAT,TLSKEYC      SET KEY CHANGED FLAG IN RECORD               
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         NI    TSINDS,FF-TSIKEYUP  TURN OFF PUT BACK REGARDLESS FLAG            
         GOTOX ASAVITEM            SAVE A COPY OF THIS RECORD                   
         OI    LSLTIND1,LSLTISHU   SET SHUFFLE REQUIRED                         
         B     EXIT                                                             
         DROP  R2,S,R3                                                          
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SAVE COPY OF TSAR RECORD TO SVLST BUFFER                 *         
*                                                                     *         
* NTRY: TSAR RECORD TO SAVE IN ATLST                                  *         
* EXIT: TSAR RECORD COPIED TO SVLST                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SAVITEM  L     RE,ATLST                                                         
         LH    RF,TLRLEN-TLSTD(RE)                                              
         LA    RF,L'TLNUM(RF)                                                   
         LA    R0,SVLST                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RE-SHUFFLE TSAR RECORDS INTO KEY SEQUENCE ORDER          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SHUFFLE  TM    LSSTAT2,LSSNOSEQ    UNIQUE TSAR KEYS?                            
         BO    SHU00               NO - SPECIAL SHUFFLE REQUIRED                
*                                                                               
         L     R3,ATSABLK                                                       
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
*                                                                               
         MVC   TSRTKLEN,=AL1(L'TLKEY)                                           
         MVI   TSRTKDSP,0                                                       
         LA    R1,TSASRT                                                        
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
*                                                                               
         MVC   LSLINE#,LSPAG#1     SET TO REDISPLAY LIST FROM LINE 1            
         GOTOX ADISPAG                                                          
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
SHU00    L     R2,ATLST            TSAR BUFFER                                  
         USING TLSTD,R2                                                         
         L     R4,AIO1             TEMPORARY BUFFER                             
N        USING TLSTD,R4                                                         
*                                                                               
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,TWASESNL     SET NEST LEVEL                               
         LA    R1,TSARDH                                                        
         B     *+8                                                              
*                                                                               
SHU02    LA    R1,TSANXT                                                        
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SHU04               END OF FILE                                  
         CLC   TLKSES,TWASESNL     CHECK NEST LEVEL                             
         BNE   SHU04               DONE ALL FOR THIS LEVEL                      
*                                                                               
         NI    TLSTAT,FF-TLSKEYC   TURN OFF KEY CHANGED FLAG                    
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         MVC   N.TLKEY,TLKEY       MAKE A COPY OF THIS KEY                      
         LH    R0,TLNUM            SAVE CURRENT LIST NUMBER                     
*                                                                               
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSARDH',N.TLSTD)                     
         BL    *+14                EOF - NOTHING THERE (NO DUPLICATION)         
         CLC   TLKEY(TLKSEQ-TLKEY),N.TLKEY                                      
         BE    SHU12               ERROR - KEY IS DUPLICATED                    
*                                                                               
         GOTOX ('TSARIO',AGROUTS),TSAADD  ADD TEMPORARY COPY OF RECORD          
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$TOMNY)                                           
         B     EXITL               ERROR ON ADD                                 
*                                                                               
         STH   R0,TLNUM            RE-GET ORIGINAL FOR READ SEQUENCE            
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
         B     SHU02               NEXT IN LIST                                 
*                                                                               
SHU04    NI    LSLTIND1,FF-LSLTISHU TURN OFF SHUFFLE REQUIRED FLAG              
*                                                                               
SHU06    XC    TLKEY,TLKEY         DELETE ALL CURRENT RECORDS                   
         MVC   TLKSES,TWASESNL                                                  
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SHU08               END OF FILE                                  
         CLC   TLKSES,TWASESNL     SAME NEST LEVEL?                             
         BNE   SHU08               NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         B     SHU06                                                            
*                                                                               
SHU08    XC    TLKEY,TLKEY         MOVE SHUFFLED TO CURRENT SESSION             
         MVI   TLKSES,TLKSKEYC     TEMPORARY SHUFFLED NEST LEVEL                
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SHU10               END OF FILE                                  
         CLI   TLKSES,TLKSKEYC     STILL TEMPORARY LEVEL?                       
         BNE   SHU10               NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         MVC   TLKSES,TWASESNL     RESTORE TO CURRENT NEST LEVEL                
         GOTOX (RF),TSAADD                                                      
         B     SHU08                                                            
*                                                                               
SHU10    MVC   LSLINE#,LSPAG#1     SET TO REDISPLAY LIST FROM LINE 1            
         GOTOX ADISPAG                                                          
         B     EXITOK                                                           
*                                                                               
SHU12    XC    TLKEY,TLKEY         DELETE ALL SHUFFLED RECORDS                  
         MVI   TLKSES,TLKSKEYC                                                  
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SHU14                                                            
         CLI   TLKSES,TLKSKEYC                                                  
         BNE   SHU14                                                            
         GOTOX (RF),TSADEL                                                      
         B     SHU12                                                            
*                                                                               
SHU14    STH   R0,LSLINE#          TSAR RECORD NUMBER DUPLICATED                
         CH    R0,LSPAG#1          ERROR ON CURRENT PAGE?                       
         BL    *+12                NO - BEFORE                                  
         CH    R0,LSPAG#X          ERROR AFTER END OF CURRENT PAGE?             
         BNH   SNO10               YES                                          
*                                                                               
         OC    LSNUMPAG,LSNUMPAG   ANY RECORDS ON PAGE?                         
         BZ    SNO08               NO                                           
         LH    R0,LSPAG#1          SEARCH FOR DUPLICATE ON PAGE                 
*                                                                               
SHU16    STH   R0,TLNUM                                                         
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
         CLC   TLKSRT,N.TLKSRT                                                  
         BE    SNO10               FOUND DUPLICATE ENTRY                        
         AH    R0,=H'1'                                                         
         CH    R0,LSPAG#X                                                       
         BNH   SHU16                                                            
SNO08    LH    R0,LSPAG#1                                                       
*                                                                               
SNO10    SH    R0,LSPAG#1                                                       
         MH    R0,LSLINLEN                                                      
         AH    R0,LS1STINP                                                      
         A     R0,ATWA                                                          
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$DUPEN)                                           
         GOTOX AGETITEM,LSLINE#                                                 
         GOTOX AGEN,RTPARM,ODATA,DNDIS,LSLIN+(LINFLD#-LINTABD),LCAREC           
         MVC   FVXTRA,FVIFLD                                                    
         B     EXITL                                                            
         DROP  R2,N                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE FOR NEW SCREEN                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
INISCR   TM    LSSTAT2,LSSSIZE     TEST SIZING SCREEN TO LIST SIZE              
         BZ    ISCR10                                                           
*                                                                               
         XC    LSREQROW,LSREQROW   RESET NUMBER OF ROWS REQUESTED               
*                                                                               
         TM    LSSTAT1,LSSBALL     BUILDING ENTIRE LIST?                        
         BZ    ISCR10              NO                                           
*                                                                               
         ICM   R1,15,BCEFFS        REQUEST ENTIRE LIST TO BE BUILT              
         GOTOX ABLDLST                                                          
         BL    EXITL               ERROR DURING BUILD                           
*                                                                               
         LA    RF,1                                                             
         TM    LSLTIND1,LSLTISOL   ANYTHING IN THE LIST?                        
         BZ    ISCR02              NO                                           
*                                                                               
         LH    RF,LSLST#X          RF=NUMBER OF ITEMS IN LIST + 1               
         SH    RF,LSLST#1                                                       
         LA    RF,1(RF)                                                         
*                                                                               
ISCR02   TM    LSSTAT1,LSSMULIN    TEST MULTIPLE RECORDS PER LINE               
         BZ    ISCR04              NO                                           
         XR    RE,RE                                                            
         LH    R1,LSLINROW                                                      
         DR    RE,R1                                                            
         LA    RF,1(RF)                                                         
         B     ISCR08                                                           
*                                                                               
ISCR04   TM    LSSTAT1,LSSMUROW    TEST MULTIPLE LINES PER RECORD               
         BZ    ISCR06              NO                                           
         MH    RF,LSROWLIN                                                      
*                                                                               
ISCR06   TM    LSSTAT3,LS3RVAR     TEST SELF-DEFINING COUNT                     
         BZ    ISCR08              NO                                           
         ICM   RF,15,GCFULL2       NUMBER OF LINES REQUIRED SAVED HERE          
         BNZ   *+8                                                              
         LA    RF,1                                                             
*                                                                               
ISCR08   STH   RF,LSREQROW         SAVE NUMBER OF REQUIRED ROWS                 
*                                                                               
ISCR10   GOTOX ABLDTOP             BUILD TOP OF SCREEN                          
         LH    RF,LSNUMHED                                                      
         MH    RF,LSCOLROW                                                      
         AH    RF,LSHEDAD                                                       
         STH   RF,LSTTPAD                                                       
         LH    RE,LSTOTTOP                                                      
         MH    RE,LSCOLROW                                                      
         AR    RF,RE                                                            
         STH   RF,LSLINAD          SAVE SCREEN ADDRESS OF 1ST LIST DATA         
*                                                                               
         GOTOX ABLDBOT             BUILD BOTTOM OF SCREEN                       
*                                                                               
         LH    RF,LSBOTAD          GET NUMBER OF ROWS LEFT FOR LIST             
         SH    RF,LSLINAD                                                       
*                                                                               
         XR    RE,RE                                                            
         LH    R0,LSCOLROW                                                      
         DR    RE,R0                                                            
         STH   RF,LSROWPAG         NUMBER OF ROWS AVAILABLE                     
*                                                                               
         MVC   LSLINPAG,LSROWPAG   SET NUMBER OF LIST LINES TO BUILD            
*                                                                               
         TM    LSSTAT1,LSSMULIN    MULTIPLE RECORDS PER LINE                    
         BZ    ISCR14                                                           
         LH    RF,LSROWPAG         NUMBER OF LINES ON SCREEN                    
         MH    RF,LSLINROW         NUMBER OF RECORDS PER LINE                   
         STH   RF,LSLINPAG         NUMBER OF RECORDS ON THIS PAGE               
         B     ISCR16                                                           
*                                                                               
ISCR14   TM    LSSTAT1,LSSMUROW    MULTIPLE LINES PER RECORD                    
         BO    *+12                YES                                          
         TM    LSSTAT3,LS3RFIX     FIXED REPEATED LINES PER RECORD              
         BZ    ISCR16              YES                                          
*                                                                               
         LH    RE,LSROWPAG         ROWS AVAILABLE                               
         SRDL  RE,32                                                            
         LH    R0,LSROWLIN         ROWS REQUIRED FOR A SINGLE LINE              
         DR    RE,R0                                                            
         STH   RF,LSLINPAG         RECORDS ON THIS PAGE                         
         B     ISCR16                                                           
*                                                                               
ISCR16   LH    RE,GSDSPMAX         SIZE OF SCREEN                               
         SH    RE,LSBOTSIZ         SIZE OF BOTTOM OF SCREEN                     
         SH    RE,LSHEDDSP         SIZE OF TOP OF SCREEN                        
         SRDL  RE,32                                                            
         LH    R1,LSLINPAG                                                      
         LA    R1,1(R1)            ALLOW FOR HEADLINES                          
         DR    RE,R1                                                            
         STH   RF,LSLINMAX         MAX LENGTH OF SINGLE FIXED LINE              
*                                                                               
         XC    LSVARLHS,LSVARLHS                                                
         GOTOX ABLDSCR,BSWRIGHT                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD THE TOP OF THE SCREEN                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDTOP   LH    R4,GSDSPOVR                                                      
         A     R4,ATWA                                                          
         USING FHD,R4                                                           
*                                                                               
         TM    LSSTAT1,LSSMAIN     MAINTENANCE SCREEN?                          
         BO    BTOP02              YES                                          
         GOTOX AGEN,RTPARM,OSCRN,SLSTTOP                                        
*                                                                               
BTOP02   MVC   LSCLMFDR,GS#FDR     SET NUMBER OF FIRST COLUMN FDREL             
*                                                                               
         MVC   LCHALF,BCEFFS       END OF SCREEN IF REAL LIST                   
         TM    LSSTAT1,LSSMAIN                                                  
         BZ    *+10                                                             
         MVC   LCHALF,GSLSTTOP     START OF LIST AREA IF MAINT. LIST            
*                                                                               
         LA    R4,TWASCR           FIND FIRST QUALIFIER USING R4                
         XR    RF,RF                                                            
         XR    R3,R3               SAVE LAST SCREEN ADDRESS IN R3               
*                                                                               
BTOP04   ICM   RF,1,FHLN           END OF SCREEN?                               
         BZ    BTOP06                                                           
         CLC   FHAD,LCHALF         BUILD AREA START?                            
         BNL   BTOP06                                                           
         ICM   R3,3,FHAD           CURRENT SCREEN ADDRESS                       
         BXH   R4,RF,BTOP04                                                     
         DROP  R4                                                               
*                                                                               
BTOP06   S     R4,ATWA             SAVE DISPLACEMENT TO FIRST HEADLINE          
         STH   R4,LSHEDDSP         WITHIN TWA                                   
*                                                                               
         AH    R3,LSCOLROW         SET R3 TO DATA ADDRESS OF LIST TOP           
         TM    LSSTAT1,LSSMAIN                                                  
         BZ    *+8                                                              
         ICM   R3,3,GSLSTTOP       OR ADDRESS OF BUILD BLOCK                    
*                                                                               
         XR    R2,R2               SET DATA SCREEN ADDRESS OF HEADLINE          
         LH    RE,LSCOLROW                                                      
         DR    R2,RE                                                            
         MH    R3,LSCOLROW         FIRST COLUMN OF THE LINE                     
*                                                                               
         TM    LSSTAT2,LSSBDR      TEST BORDER WITH THIS LIST                   
         BZ    *+12                NO                                           
         STH   R3,LSBDRTOP                                                      
         AH    R3,LSCOLROW                                                      
         STH   R3,LSHEDAD          ADDRESS OF FIRST HEADLINE                    
*                                                                               
         XR    RF,RF                                                            
         OC    GSDSPREC,GSDSPREC   RECORD FIELD?                                
         BNZ   *+8                                                              
         LA    RF,GSDSPREC         DISP TO RECORD FIELD                         
         OC    GSDSPACT,GSDSPACT   ACTION FIELD                                 
         BNZ   *+8                                                              
         LA    RF,GSDSPACT         DISP TO ACTION FIELD                         
         MVC   LS1STKEY,0(RF)      SET CURSOR DEFAULT IF NO INPUT FIELD         
*                                                                               
         L     R4,ATWA                                                          
         AH    R4,GSDSPOVR                                                      
         USING FHD,R4                                                           
         XR    RF,RF                                                            
*                                                                               
BTOP08   ICM   RF,1,FHLN           LOOK FOR FIRST INPUT FIELD ON SCREEN         
         BZ    BTOP10                                                           
         TM    FHAT,FHATPR                                                      
         BZ    *+8                                                              
         BXH   R4,RF,BTOP08                                                     
         S     R4,ATWA                                                          
         STH   R4,LS1STKEY                                                      
         DROP  R4                                                               
*                                                                               
BTOP10   TM    LSSTAT1,LSSMAIN     MAINTENANCE LIST?                            
         BZ    BLDTOPX             NO                                           
*                                                                               
         LA    R4,TWASCR                                                        
         USING FHD,R4                                                           
         XR    RF,RF                                                            
BTOP12   ICM   RF,1,FHLN           END OF SCREEN?                               
         BZ    BLDTOPX             YES - NOTHING AFTER END OF LIST              
*                                                                               
         CLC   FHAD,GSLSTEND       END OF LIST YET?                             
         BNL   *+8                 YES                                          
         BXH   R4,RF,BTOP12                                                     
*                                                                               
         LR    RE,R4               RE = A(END OF LIST)                          
         ICM   RF,1,FHLN                                                        
         BZ    *+8                 END OF SCREEN                                
         BXH   R4,RF,*-8                                                        
*                                                                               
         LH    R0,LSHEDDSP         R0 = A(FIRST FIELD IN LIST)                  
         A     R0,ATWA                                                          
         CR    R0,RE               SAME ADDRESS?                                
         BE    BLDTOPX             YES                                          
*                                                                               
         LR    RF,R4                                                            
         SR    RF,RE               RF = L'PORTION AT END OF SCREEN              
         LR    R1,RF                                                            
*                                                                               
         LR    R4,R0                                                            
         LA    R4,0(RF,R4)         R4 = NEW END OF SCREEN AFTER MVCL            
         MVCL  R0,RE                                                            
         MVC   0(L'EOS,R4),EOS     SET END OF SCREEN                            
*                                                                               
BLDTOPX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD BOTTOM OF SCREEN                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDBOT   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         MVC   LSBOTDSP,LSHEDDSP   INITIAL DISP. TO BOTTOM OF SCREEN            
*                                                                               
         TM    LSSTAT1,LSSMAIN     TEST MAINTENANCE SCREEN                      
         BO    BBOT02                                                           
         GOTOX AGEN,RTPARM,OSCRN,SLSTBOT ??                                     
*                                                                               
BBOT02   L     R4,ATWA                                                          
         AH    R4,LSBOTDSP                                                      
         USING FHD,R4                                                           
         CLI   FHD,0               TEST ANY BOTTOM OF SCREEN                    
         BE    BBOT10                                                           
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         ICM   R3,3,FHAD                                                        
         TM    LSSTAT1,LSSMAIN     TEST MAINTENACE SCREEN                       
         BZ    *+12                                                             
         ICM   R3,3,GSLSTEND                                                    
         AH    R3,LSCOLROW                                                      
         LH    R0,LSCOLROW                                                      
         DR    R2,R0                                                            
         MH    R3,LSCOLROW                                                      
         STH   R3,LSBOTAD          SAVE DATA SCREEN ADDRESS                     
         XR    RF,RF               FIND END OF SCREEN                           
         ICM   RF,1,FHLN                                                        
         BZ    *+12                                                             
         ICM   R3,3,FHAD                                                        
         BXH   R4,RF,*-12                                                       
         XR    R2,R2               SAVE FOOTLINE DATA SCREEN ADDRESS            
         LH    R0,LSCOLROW                                                      
         DR    R2,R0                                                            
         MH    R3,LSCOLROW                                                      
         AH    R3,LSCOLROW                                                      
         STH   R3,LSFOOAD                                                       
         B     BBOT20                                                           
*                                                                               
BBOT10   LH    RE,LSNUMFTL         NO SCREEN - CALC DATA SCREEN ADDRESS         
         MH    RE,LSCOLROW                                                      
         LA    RF,COLS#Q*ROWS#Q                                                 
         SR    RF,RE                                                            
         STH   RF,LSFOOAD                                                       
         LH    RE,LSTOTBOT                                                      
         MH    RE,LSCOLROW                                                      
         SR    RF,RE                                                            
         STH   RF,LSTBTAD                                                       
         STH   RF,LSBOTAD                                                       
*                                                                               
BBOT20   TM    LSSTAT2,LSSSIZE     TEST SIZING                                  
         BZ    BBOT30                                                           
         OC    LSREQROW,LSREQROW                                                
         BZ    BBOT30                                                           
         LH    RF,LSLINAD                                                       
         LH    RE,LSREQROW                                                      
         MH    RE,LSCOLROW                                                      
         AH    RF,LSCOLROW                                                      
         LA    RF,0(RE,RF)                                                      
         CH    RF,LSBOTAD                                                       
         BH    BBOT30               ??AATK                                      
         STH   RF,LSFOOAD                                                       
         LH    RE,LSTOTBOT                                                      
         MH    RE,LSCOLROW                                                      
         SR    RF,RE                                                            
         STH   RF,LSTBTAD                                                       
         STH   RF,LSBOTAD                                                       
*                                                                               
BBOT30   TM    LSSTAT2,LSSBDR      TEST DOING A BORDER                          
         BZ    BBOT40                                                           
         MVC   LSBDRBOT,LSBOTAD                                                 
         LH    RF,LSBOTAD                                                       
         SH    RF,LSCOLROW                                                      
         STH   RF,LSBOTAD                                                       
         LH    RF,LSFOOAD                                                       
         SH    RF,LSCOLROW                                                      
         STH   RF,LSFOOAD                                                       
         LH    RF,LSTBTAD                                                       
         SH    RF,LSCOLROW                                                      
         STH   RF,LSTBTAD                                                       
*                                                                               
BBOT40   LH    R0,LSNUMFTL         R0=NO. OF FOOTLINES                          
         AH    R0,LSTOTBOT                                                      
         BZ    BBOT50                                                           
         XC    BSFOOT,BSFOOT       INITIALIZE FOOTLINE                          
F        USING FHD,BSFOOT                                                       
         LH    R5,LSCOLROW                                                      
         TM    LSSTAT2,LSSBDR      TEST FOR BORDER                              
         BZ    *+16                                                             
         CH    R5,=Y(COLS#Q-5)                                                  
         BL    *+8                                                              
         LA    R5,COLS#Q-5                                                      
*                                                                               
         LA    RF,FHDAD+FHDAD-1(R5)                                             
         STC   RF,F.FHLN                                                        
         LA    R5,F.FHDA-1(R5)     R5=HELP NUMBER BYTE                          
         MVI   F.FHAT,FHATLC+FHATPR+FHATHI+FHATXH                               
         LH    RF,LSDSPROW                                                      
         LA    RF,1(RF)                                                         
         STH   RF,F.FHAD                                                        
*                                                                               
         LA    RE,FHD              SET DISPLACEMENT TO FOOTLINE                 
         S     RE,ATWA                                                          
         STH   RE,LSTBTDSP                                                      
         LH    RF,LSTOTBOT                                                      
         XC    LCHALF,LCHALF                                                    
         MVC   LCHALF+1(1),F.FHLN                                               
         MH    RF,LCHALF                                                        
         AR    RE,RF                                                            
         STH   RE,LSFOODSP                                                      
*                                                                               
         LH    RF,LSTBTAD          SET BLDROW PARMS                             
         GOTOX ,RTPARM,(RF),FHD,BSFOOT                                          
*                                                                               
         L     RF,ABLDROW                                                       
BBOT42   LNR   RE,R0               SET HELP NUMBER                              
         STC   RE,0(R5)                                                         
         GOTOX (RF),(R1),,,        ADD FOOTLINE                                 
         BCT   R0,BBOT42                                                        
         DROP  F                                                                
         L     R4,4(R1)            R4=A(END OF TWA)                             
*                                                                               
BBOT50   TM    LSSTAT2,LSSBDR      TEST FOR BORDER                              
         BZ    BBOT52                                                           
         GOTOX ABORDER,RTPARM,FHD  DRAW IT                                      
         L     R4,0(R1)                                                         
*                                                                               
BBOT52   MVC   FHD(L'EOS),EOS                                                   
         S     R4,ATWA             SET SIZE OF BOTTOM OF SCREEN                 
         SH    R4,LSHEDDSP                                                      
         STH   R4,LSBOTSIZ                                                      
         CLC   LSBOTSIZ,=Y(L'BSBOT)                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
BLDBOTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD THE LIST                                           *         
*                                                                     *         
* NTRY: R1 = HIGHEST NUMBER REQUIRED FOR LIST                         *         
* EXIT: LIST BUILT AT LEAST UP TO NUMBER REQUIRED                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDLST   STCM  R1,3,BLLST#X        SAVE LIST HIGH NUMBER                        
         TM    LSLTIND1,LSLTIBLD   LIST STARTED?                                
         BO    BLST02              YES                                          
*                                                                               
         XC    LSLASKEY,LSLASKEY   RESET SAVED KEY                              
         MVC   LCRECKEY,LSINIKEY   SET INITIAL KEY                              
*                                                                               
         GOTOX ALST,RTPARM,OLIST,LLSTFRST,LCRECKEY                              
         BL    EXITL               ERROR ON FIRST FOR LIST                      
         B     *+10                                                             
*                                                                               
BLST02   MVC   LCRECKEY,LSLASKEY   RESTORE SAVED KEY                            
*                                                                               
         TM    LSLTIND1,LSLTIEOL   END-OF-LIST REACHED?                         
         BO    BLDLSTX             YES                                          
         CLC   LSLST#X,BLLST#X     HIGHEST RECORD ALREADY IN LIST?              
         BNL   BLDLSTX             YES                                          
*                                                                               
BLST10   LA    RF,LGETFRST         GET FIRST/NEXT RECORD                        
         B     *+8                                                              
BLST12   LA    RF,LGETNEXT                                                      
*                                                                               
         GOTOX ALST,RTPARM,OLIST,(RF),LCRECKEY,LSLASKEY,LCEQUREC                
         BNE   BLST20              END OF LIST                                  
*                                                                               
         TM    LSSTAT1,LSSBALL     BUILDING ALL OF LIST?                        
         BO    *+14                YES                                          
         CLC   LSLST#X,BLLST#X     HIGH RECORD FOUND?                           
         BNL   BLDLSTX             YES                                          
*                                                                               
         TM    LSSTAT3,LS3RVAR     TSAR RECORDS DEFINE ROWS REQUIRED            
         BZ    BLST14              NO                                           
         L     RF,ATLST                                                         
         XR    RE,RE                                                            
         ICM   RE,1,TLROWS-TLSTD(RF)                                            
         BNZ   *+8                                                              
         LA    RE,1                                                             
         A     RE,GCFULL2                                                       
         ST    RE,GCFULL2          SAVE NEW ROW REQUIREMENT                     
*                                                                               
BLST14   GOTOX ALST,RTPARM,OLIST,LTSARADD                                       
         BE    BLST12                                                           
         TM    LSSTAT2,LSTSFULL                                                 
         BZ    EXITL                                                            
         MVC   FVMSGNO,=AL2(GE$TOMNY)                                           
         B     EXITL                ERROR ADDING TO LIST                        
*                                                                               
BLST20   GOTOX ALST,RTPARM,OLIST,LLSTLAST                                       
         BL    EXITL                                                            
         OI    LSLTIND1,LSLTIEOL   END-OF-LIST FOUND                            
*                                                                               
BLDLSTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD THE SCREEN                                         *         
*                                                                     *         
* NTRY: R1 = BSWLEFT TO BUILD SCREEN LEFTWARDS FROM LSVARRHS          *         
*       R1 = BSWRIGHT TO BUILD SCREEN RIGHTWARDS FROM LSVARLHS        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDSCR   STC   R1,BSWAY            SAVE DIRECTION OF SCREEN BUILD               
         GOTOX ASAVBOT             SAVE BOTTOM OF SCREEN                        
*                                                                               
         XR    RF,RF                                                            
         IC    RF,LSCLMFDR                                                      
         GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'C',(RF))                            
         BL    EXITL               ERROR CLEARING LIST FDRELS                   
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         GOTOX ABLDFIX             BUILD FIXED COLUMNS                          
         GOTOX ABLDVAR,BSWAY       BUILD VARIABLE COLUMNS                       
*                                                                               
BSCR02   LA    RE,BSLIN            SAVE LINE TABLE                              
         LA    RF,LSLINS                                                        
         LA    R0,LSLIN                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   BSWAY,BSWLEFT       TEST BUILDING LEFTWARDS                      
         BNE   BSCR04                                                           
         GOTOX ABLDREV             YES - REVERSE ORDER OF VARIABLE COLS         
*                                                                               
BSCR04   TM    LSSTAT2,LSSXSPAC    SPREAD OUT VARIABLE COLUMNS?                 
         BO    BSCR06              NO                                           
         GOTOX ABLDSPC                                                          
*                                                                               
BSCR06   GOTOX ABLDHED             BUILD HEADLINES                              
         GOTOX ABLDLIN             BUILD LIST LINE                              
*                                                                               
         LH    RF,LSLINPAG              NUMBER OF LIST LINES                    
         MH    RF,LSLINLEN            * LENGTH OF 1 SCREEN LINE                 
*                                                                               
         AH    RF,LSHEDDSP            + TOP OF SCREEN                           
         AH    RF,LSHEDLEN            + HEAD LINES                              
         AH    RF,LSBOTSIZ            + BOTTOM OF SCREEN                        
         CH    RF,GSDSPMAX         TEST SCREEN SIZE WITHIN MAXIMUM              
         BL    BSCR08                                                           
*                                                                               
         LA    RE,LSLIN            RESTORE LINE TABLE                           
         LA    RF,LSLINS                                                        
         LA    R0,BSLIN                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         GOTOX ADELLIN             DELETE LAST LINE TABLE ENTRY                 
         B     BSCR02              TRY AGAIN                                    
*                                                                               
BSCR08   LH    R2,LSHEDAD          R2=HEADLINE DATA SCREEN ADDRESS              
         LH    R3,LSHEDDSP         R3=HEADLINE TWA ADDRESS                      
         A     R3,ATWA                                                          
*                                                                               
         GOTOX ABLDROW,RTPARM,(R2),(R3),BSHEAD1                                 
*                                                                               
         CLC   LSNUMHED,=AL2(1)    ONE HEADLINE?                                
         BE    BSCR10              YES                                          
         GOTOX (RF),(R1),,,BSHEAD2 ADD HEADLINES 1 & 2                          
*                                                                               
BSCR10   XR    R0,R0                                                            
         ICM   R0,3,LSTOTTOP       NUMBER OF TOTAL LINES AT TOP                 
         BZ    BSCR14              NONE                                         
*                                                                               
         L     RF,4(R1)                                                         
         S     RF,ATWA                                                          
         STH   RF,LSTTPDSP         DISPLACEMENT TO FIRST TOP TOTAL LINE         
T        USING FHD,BSFOOT                                                       
         XC    BSFOOT,BSFOOT (ALTHOUGH NOT REALLY A FOOTLINE)                   
*                                                                               
         LH    R5,LSCOLROW                                                      
         LA    RF,FHDAD+FHDAD-1(R5)                                             
         STC   RF,T.FHLN                                                        
         LA    R5,T.FHDA-1(R5)     R5=HELP NUMBER BYTE                          
         MVI   T.FHAT,FHATLC+FHATPR+FHATHI+FHATXH                               
         LH    RF,LSDSPROW                                                      
         LA    RF,1(RF)                                                         
         STH   RF,T.FHAD                                                        
         GOTOX ,(R1),,,BSFOOT      SET BLDROW PARMS                             
         L     RF,ABLDROW                                                       
*                                                                               
BSCR12   STC   R0,0(R5)            SET HELP NUMBER                              
         GOTOX (RF),(R1),,,        ADD FOOTLINE                                 
         BCT   R0,BSCR12                                                        
         DROP  T                                                                
*                                                                               
BSCR14   L     R2,4(R1)            CURRENT END OF TWA RETURNED                  
         S     R2,ATWA                                                          
         STH   R2,LS1STLIN         DISPLACEMENT TO FIRST LIST LINE              
*                                                                               
         LH    R0,LSLINPAG         R0=NO. OF LINES FITTING ON SCREEN            
         L     R3,0(R1)                                                         
*                                                                               
         TM    LSSTAT1,LSSMULIN    MULTIPLE RECORDS PER LINE?                   
         BZ    *+8                 NO                                           
         LH    R0,LSROWPAG         NUMBER OF LINES ON THE SCREEN                
*                                                                               
         LH    R5,LSCOLROW         R5=DISP. TO NEXT LINE                        
         TM    LSSTAT1,LSSMUROW    MULTIPLE ROWS PER SINGLE RECORD?             
         BZ    *+8                 NO                                           
         LH    R5,LSCOLLIN                                                      
*                                                                               
BSCR16   LA    R4,1                                                             
         TM    LSSTAT3,LS3RFIX     REPEATED SINGLE ROWS PER RECORD              
         BZ    *+8                                                              
         LH    R4,LSROWLIN                                                      
*                                                                               
BSCR18   L     R2,4(R1)                                                         
         GOTOX (RF),(R1),(R3),,BSLINE                                           
*                                                                               
         TM    LSSTAT3,LS3RFIX     REPEATED SINGLE ROWS PER RECORD              
         BZ    BSCR20              NO                                           
         CH    R4,LSROWLIN         FIRST OF REPEATED ROWS?                      
         BE    BSCR20              YES                                          
         OC    LSSUBLEN,LSSUBLEN   SUB-ACTION FIELD?                            
         BZ    BSCR20              NO                                           
         OI    FHAT-FHD(R2),FHATPR PROTECT SUB-ACTION FIELD                     
*                                                                               
BSCR20   AR    R3,R5                                                            
         BCT   R4,BSCR18           DO FOR ALL REPEATED COPIES                   
*                                                                               
         BCT   R0,BSCR16           DO FOR ALL SCREEN LINES                      
*                                                                               
         L     R2,4(R1)            RESTORE BOTTOM OF SCREEN                     
         GOTOX ARESBOT,RTPARM,(R2)                                              
         L     R2,0(R1)            R2=A(END OF SCREEN)                          
         MVC   0(L'EOS,R2),EOS     SET END OF SCREEN BITS                       
*                                                                               
         XR    RF,RF               TRANSMIT SCREEN                              
         LA    R2,TWASCR                                                        
         USING FHD,R2                                                           
         ICM   RF,1,FHLN                                                        
         BZ    *+12                                                             
         OI    FHOI,FHOITR                                                      
         BXH   R2,RF,*-12                                                       
         DROP  R2                                                               
*                                                                               
         MVC   LS1STINP,LS1STLIN                                                
         L     RE,ATWA             SAVE A(1ST INPUT FIELD ON LINE)              
         AH    RE,LS1STLIN                                                      
         USING FHD,RE                                                           
         TM    FHAT,FHATPR                                                      
         BZ    BLDSCRX                                                          
         XR    R0,R0                                                            
         LR    R1,RE                                                            
         BCTR  R1,0                                                             
         AH    R1,LSLINLEN                                                      
         TM    FHAT,FHATPR                                                      
         BZ    *+16                                                             
         IC    R0,FHLN                                                          
         BXLE  RE,R0,*-12                                                       
         B     *+12                                                             
         S     RE,ATWA                                                          
         STH   RE,LS1STINP                                                      
         DROP  RE                                                               
*                                                                               
BLDSCRX  GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SPARE FOR ANYTHING TO HAVE                                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
NXTSCRN  DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LINE TABLE FOR FIXED COLUMNS                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDFIX   LA    RE,LSLIN            RESET LINE TABLE                             
         LA    RF,LSLINS                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   LSLIN,LINTEOT                                                    
         XC    ALPARM,ALPARM       RESET ADDLIN PARAMETERS                      
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,LSFIXNUM       ANY FIXED COLUMNS TO BUILD?                  
         BZ    BFIXX               NO                                           
*                                                                               
         LA    R1,LSFIXCLM         A(FIRST FIXED COLUMN)                        
BFIX02   GOTOX AADDLIN             ADD FIXED COLUMNS TO LINE                    
         BNE   BFIXX               END OF LINE REACHED                          
         LA    R1,DCTABL(R1)                                                    
         BCT   R0,BFIX02           DO FOR ALL FIXED COLUMNS                     
*                                                                               
BFIXX    MVC   LSFIXCOL,ALCURCOL   SAVE CURRENT COLUMN POSITION                 
         MVC   LSFIXLEN,ALCURLEN   SAVE CURRENT LENGTH OF LINE                  
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD VARIABLE COLUMNS TO LINE TABLE                       *         
*                                                                     *         
* NTRY: R1 = A(BSWRIGHT/BSWLEFT) TO BUILD RIGHT/LEFTWARDS             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDVAR   XC    LSVARPAG,LSVARPAG                                                
         OC    LSVARNUM,LSVARNUM                                                
         BZ    BLDVARX                                                          
         XC    ALPARM,ALPARM       SET UP ADDLIN PARAMETER LIST                 
         MVC   ALCURCOL,LSFIXCOL                                                
         MVC   ALCURLEN,LSFIXLEN                                                
         OI    ALINDS,ALIVAR       ADDING VARIABLE COLUMNS                      
         LH    RE,LSFIXNUM         SET DISP. TO L-H-S LINTABD ENTRY             
         MH    RE,=Y(LINTABL)                                                   
         STH   RE,LSLINLHS                                                      
         LA    RE,LSLIN(RE)                                                     
         ST    RE,ALALIN                                                        
*                                                                               
         CLI   0(R1),BSWRIGHT      TEST BULILDING RIGHTWARDS                    
         BNE   BVAR10                                                           
         LH    R2,LSVARLHS         R2=L-H-S SEQUENCE NUMBER                     
         LR    R1,R2                                                            
         MH    R1,=Y(DCTABL)                                                    
         LA    R1,LSVARCLM(R1)     R1=DISPLAYABLE COLUMN ENTRY                  
*                                                                               
BVAR02   GOTOX AADDLIN             ADD LINE TABLE ENTRY                         
         BNE   BVAR04                                                           
         STH   R2,LSVARRHS         SAVE R-H-S #                                 
         LA    R1,DCTABL(R1)                                                    
         LA    R2,1(R2)                                                         
         CH    R2,LSVARNUM         TEST AT END OF LINE                          
         BL    BVAR02                                                           
BVAR04   B     BVAR20                                                           
*                                  BUILDLING LEFTWARDS                          
BVAR10   LH    R2,LSVARRHS         R2=R-H-S SEQUENCE NUMBER                     
         LR    R1,R2                                                            
         MH    R1,=Y(DCTABL)                                                    
         LA    R1,LSVARCLM(R1)     R1=DISPLAYABLE COLUMN ENTRY                  
*                                                                               
BVAR12   GOTOX AADDLIN             ADD LINE TABLE ENTRY                         
         BNE   BVAR20                                                           
         STH   R2,LSVARLHS         SAVE L-H-S #                                 
         SH    R1,=Y(DCTABL)                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2               TEST AT START OF LINE                        
         BNM   BVAR12                                                           
BVAR14   DS    0H                  - WAIT A MINUTE -                            
*                                                                               
BVAR20   LH    RF,LSVARRHS         SET NO. OF VARIABLE COLUMNS ON PAGE          
         SH    RF,LSVARLHS                                                      
         LA    RF,1(RF)                                                         
         STH   RF,LSVARPAG                                                      
         LH    RE,LSLINLHS                                                      
         BCTR  RF,0                SET DISP. TO R-H-S LINTABD ENTRY             
         MH    RF,=Y(LINTABL)                                                   
         AR    RE,RF                                                            
         STH   RE,LSLINRHS                                                      
*                                                                               
BLDVARX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REVERSE ORDER OF VARIABLE COLUMNS                        *         
***********************************************************************         
         SPACE 1                                                                
BLDREV   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         XC    ALPARM,ALPARM       SET UP ADDLIN PARAMETER LIST                 
         MVC   ALCURCOL,LSFIXCOL                                                
         MVC   ALCURLEN,LSFIXLEN                                                
         OI    ALINDS,ALIVAR       ADDING VARIABLE COLUMNS                      
         LH    RE,LSFIXNUM         SET DISP. TO L-H-S LINTABD ENTRY             
         MH    RE,=Y(LINTABL)                                                   
         STH   RE,LSLINLHS                                                      
         LA    RE,LSLIN(RE)                                                     
         ST    RE,ALALIN                                                        
*                                                                               
         LH    R2,LSVARLHS         R2=L-H-S SEQUENCE NUMBER                     
         LR    R1,R2                                                            
         MH    R1,=Y(DCTABL)                                                    
         LA    R1,LSVARCLM(R1)     R1=DISPLAYABLE COLUMN ENTRY                  
         XR    R0,R0                                                            
         ICM   R0,3,LSVARPAG                                                    
         BZ    BLDREVX                                                          
*                                                                               
BREV02   GOTOX AADDLIN             ADD LINE TABLE ENTRY                         
         BE    *+6                                                              
         DC    H'0'                UNFORTUNATELY, COULD HAPPEN                  
         LA    R1,DCTABL(R1)                                                    
         BCT   R0,BREV02                                                        
*                                                                               
BLDREVX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EVENLY SPACE OUT VARIABLE COLUMNS                        *         
***********************************************************************         
         SPACE 1                                                                
BLDSPC   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         CLC   LSVARPAG,=H'1'      TEST MORE THAN ONE VARIABLE COLUMN           
         BNH   BLDSPCX                                                          
         TM    LSSTAT1,LSSMUROW    TEST MULTIPLE ROWS PER LINE                  
         BZ    BSPC10                                                           
         LH    R2,LSLINLHS                                                      
         LA    R2,LSLIN(R2)                                                     
         USING LINTABD,R2                                                       
         LH    R3,LSCOLROW                                                      
         LR    R5,R2               R5=A(START OF LINE)                          
BSPC02   CLI   LINTABD,LINTEOT                                                  
         BE    BSPC06                                                           
         CH    R3,LINHCOL                                                       
         BH    BSPC04                                                           
         GOTOX ASPCOUT,RTPARM,(R3),(R5),(R4)                                    
*                                                                               
         LR    R5,R2               R5=A(START OF LINE)                          
         AH    R3,LSCOLROW                                                      
BSPC04   LR    R4,R2               R4=A(END OF LINE)                            
         LA    R2,LINTABL(R2)                                                   
         B     BSPC02                                                           
*                                                                               
BSPC06   GOTOX ASPCOUT,RTPARM,(R3),(R5),(R4)                                    
         B     BLDSPCX                                                          
         DROP  R2                                                               
*                                                                               
BSPC10   LH    R2,LSLINLHS                                                      
         LA    R2,LSLIN(R2)                                                     
         LH    R3,LSLINRHS                                                      
         LA    R3,LSLIN(R3)                                                     
         LH    RF,LSCOLLIN                                                      
         GOTOX ASPCOUT,RTPARM,(RF),(R2),(R3)                                    
*                                                                               
BLDSPCX  B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* SPACE OUT A ROW                                                     *         
*                                                                     *         
* NTRY: P1 = NO. OF COLUMNS IN LIST LINE                              *         
*       P2 = A(1ST LINE TABLE ENTRY) FOR ROW                          *         
*       P3 = A(LAST LINE TABLE ENTRY) FOR ROW                         *         
***********************************************************************         
         SPACE 1                                                                
SPCOUT   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         LM    R3,R5,0(R1)         R3=NO. OF COLUMNS IN ROW                     
F        USING LINTABD,R4          R4=A(FIRST LINTABD)                          
L        USING LINTABD,R5          R5=A(LAST LINTABD)                           
         LR    RF,R5                                                            
         SR    RF,R4                                                            
         BZ    SPCOUTX                                                          
         XR    RE,RE                                                            
         LA    R0,LINTABL                                                       
         DR    RE,R0                                                            
         LA    R2,1(RF)            R2=NO. OF ENTRIES                            
*                                                                               
         LH    RE,L.LINHCOL                                                     
         AH    RE,L.LINHWDTH                                                    
         LA    R1,1(R3)                                                         
         SR    R1,RE               R1=SPARE SPACE AT END OF LINE                
         BZ    SPCOUTX                                                          
         XR    R0,R0                                                            
         DR    R0,R2               R1=GAP                                       
         XR    R0,R0                                                            
*                                                                               
SOUT02   LH    RE,F.LINHCOL        SPACE HEADING POSITIONS                      
         AR    RE,R0                                                            
         STH   RE,F.LINHCOL                                                     
         AR    R0,R1                                                            
         LA    R4,LINTABL(R4)                                                   
         BCT   R2,SOUT02                                                        
*                                                                               
SPCOUTX  B     EXIT                                                             
         DROP  F,L                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD HEADLINES                                          *         
*                                                                     *         
* EXIT: HEADLINE 1 IN BSHEAD1                                         *         
*       HEADLINE 2 IN BSHEAD2                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDHED   LA    RE,BSHEAD1          CLEAR HEADLINES 1 & 2                        
         LA    RF,L'BSHEAD1+L'BSHEAD2                                           
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R3,BSHEAD1                                                       
H1       USING FHD,R3              R3=A(HEAD-LINE 1)                            
         LA    R2,BSHEAD2                                                       
H2       USING FHD,R2              R2=A(HEAD-LINE 2)                            
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,1,LSSUBLEN       SUB-ACTION FIELD ON SCREEN?                  
         BZ    BHED02              NO                                           
         LA    R5,FHDAD(R5)        YES - SET HEADLINE FOR IT                    
*                                                                               
         CLC   LSNUMHED,=H'1'      ONE HEADLINE ONLY?                           
         BNE   BHED01              NO                                           
*                                                                               
         STC   R5,H1.FHLN          SET SUB-ACTION NAME ON LINE 1                
         MVI   H1.FHAT,FHATLC+FHATPR+FHATHI                                     
         LH    RE,LSDSPROW                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,H1.FHAD                                                     
         MVI   H1.FHDA,GE#ESCL                                                  
         MVC   H1.FHDA+1(2),=AL2(GE#ACTN)                                       
         TM    LSSTAT1,LSSSEL                                                   
         BZ    *+10                                                             
         MVC   H1.FHDA+1(2),=AL2(GE#SLCT)                                       
         MVC   H1.FHDA+3(1),LSSUBLEN                                            
         GOTOX VDICTAT,RTPARM,C'SL  ',H1.FHDA                                   
         AR    R3,R5                                                            
         B     BHED02                                                           
*                                                                               
BHED01   STC   R5,H2.FHLN          SET SUB-ACTION NAME ON LINE 2                
         MVI   H2.FHAT,FHATLC+FHATPR+FHATHI                                     
         LH    RE,LSDSPROW                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,H2.FHAD                                                     
         MVI   H2.FHDA,GE#ESCL                                                  
         MVC   H2.FHDA+1(2),=AL2(GE#ACTN)                                       
         TM    LSSTAT1,LSSSEL                                                   
         BZ    *+10                                                             
         MVC   H2.FHDA+1(2),=AL2(GE#SLCT)                                       
         MVC   H2.FHDA+3(1),LSSUBLEN                                            
         GOTOX VDICTAT,RTPARM,C'SL  ',H2.FHDA                                   
         AR    R2,R5                                                            
*                                                                               
BHED02   LA    R4,LSLIN                                                         
         USING LINTABD,R4                                                       
*                                                                               
BHED04   CLI   LINTABD,LINTEOT     SET HEADLINE FOR EACH COLUMN                 
         BE    BHED20                                                           
         LH    R5,LINHWDTH                                                      
         LA    R5,FHDAD(R5)                                                     
*                                                                               
         XC    LCWORK1,LCWORK1                                                  
T1       USING FHD,LCWORK1                                                      
         XC    LCWORK2,LCWORK2                                                  
T2       USING FHD,LCWORK2                                                      
                                                                                
         STC   R5,T1.FHLN                                                       
         MVI   T1.FHAT,FHATLC+FHATPR+FHATHI                                     
         LH    RE,LINHCOL                                                       
         AH    RE,LSDSPROW                                                      
         STCM  RE,3,T1.FHAD                                                     
*                                                                               
         LH    RF,LINFLD#                                                       
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     RE,AFDRADDR                                                      
         L     RF,0(RE,RF)                                                      
         MVC   T1.FHIL,FDRTFHXT-FDRELD(RF)                                      
         CLI   T1.FHIL,0                                                        
         BE    *+8                                                              
         OI    T1.FHII,FHIXAT                                                   
         EX    R5,*+4                                                           
         MVC   T2.FHD(0),T1.FHD                                                 
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH BACK TO APP NATIVE SYS         
         LH    RF,LINFLD#                                                       
         GOTOX AGEN,RTPARM,ODATA,DHED,(RF),T1.FHD,T2.FHD                        
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
*                                                                               
         TM    LSSTAT1,LSSMUROW    TEST MULTIPLE ROWS PER LINE                  
         BZ    BHED14              NO                                           
         TM    LININDS,LINIVAR     TEST VARIABLE COLUMN                         
         BZ    BHED14              NO                                           
         TM    LSSTAT3,(LS3RFIX+LS3RVAR)                                        
         BNZ   BHED14              SPECIAL REPEATED ROWS                        
*                                                                               
         CLC   LINHCOL,LSCOLROW     TEST COLUMN ON FIRST LINE                   
         BH    BHED12                                                           
         EX    R5,*+4              COPY TO TOP HEADLINE                         
         MVC   H1.FHD(0),T1.FHD                                                 
         AR    R3,R5                                                            
         B     BHED18                                                           
*                                                                               
BHED12   EX    R5,*+4              COPY TO BOTTOM HEADLINE                      
         MVC   H2.FHD(0),T1.FHD                                                 
         ICM   RE,3,H2.FHAD                                                     
         SH    RE,LSCOLROW                                                      
         STCM  RE,3,H2.FHAD                                                     
         AR    R2,R5                                                            
         B     BHED18                                                           
*                                                                               
BHED14   EX    R5,*+4              COPY HEADLINES 1 & 2                         
         MVC   H1.FHD(0),T1.FHD                                                 
         AR    R3,R5                                                            
         EX    R5,*+4                                                           
         MVC   H2.FHD(0),T2.FHD                                                 
         AR    R2,R5                                                            
         DROP  T1,T2                                                            
*                                                                               
BHED18   LA    R4,LINTABL(R4)                                                   
         B     BHED04                                                           
         DROP  R4                                                               
*                                                                               
BHED20   TM    LSSTAT2,LSS1HEAD    TEST SQUASH INTO 1 HEADLINE                  
         BZ    BHED30                                                           
         GOTOX SQUHED,RTPARM,BSHEAD1,H1.FHD                                     
         L     R3,4(R1)                                                         
         GOTOX (RF),(R1),BSHEAD2,H2.FHD                                         
         L     R2,4(R1)                                                         
         GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH BACK TO APP NATIVE SYS         
         GOTOX AOLY,RTPARM,OLIST,LSETHEAD,BSHEAD1,BSHEAD2                       
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
*                                                                               
BHED30   MVI   H1.FHD,0                                                         
         MVI   H2.FHD,0                                                         
         LA    RE,BSHEAD1          SAVE TOTAL LENGTH OF BOTH HEADLINES          
         SR    R3,RE                                                            
         LA    RE,BSHEAD2                                                       
         SR    R2,RE                                                            
         AR    R2,R3                                                            
         STH   R2,LSHEDLEN                                                      
*                                                                               
         TM    LSSTAT1,LSSMULIN    TEST MULTIPLE LINES PER ROW                  
         BZ    BLDHEDX                                                          
         GOTOX ABLDMUL,BSHEAD1     MULTIPLY HEADLINE 1                          
         GOTOX (RF),BSHEAD2        MULTIPLY HEADLINE 2                          
         LH    RF,LSLINROW         MULTIPLY HEADLINE LENGTH                     
         MH    RF,LSHEDLEN                                                      
         STH   RF,LSHEDLEN                                                      
*                                                                               
BLDHEDX  B     EXIT                                                             
         DROP  H1,H2                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO SQUASH HEADERS INTO ONE FIELD                            *         
*                                                                     *         
* NTRY: P1 = A(START OF HEADLINE)                                     *         
*       P2 = A(END OF HEADLINE)                                       *         
* EXIT: P2 = A(NEW END OF HEADLINE)                                   *         
***********************************************************************         
         SPACE 1                                                                
SQUHED   NTR1  ,                                                                
         LR    R5,R1               R5=A(PARAMETERS)                             
         LM    R3,R4,0(R5)                                                      
H        USING FHD,R3              R3=A(HEADLINE)                               
         MVI   0(R4),0             ENSURE 0 AT END OF HEADLINE                  
         LR    R4,R3               SAVE START OF HEADLINE IN R4                 
*                                                                               
         XC    LCWORK1,LCWORK1                                                  
         XC    LCWORK2,LCWORK2                                                  
S        USING FHD,LCWORK1         SET UP 'SQUASHED' FIELD HEADER               
         LH    RF,LSCOLROW                                                      
         LA    RF,FHDAD-1(RF)                                                   
         STC   RF,S.FHLN                                                        
         MVI   S.FHAT,FHATLC+FHATPR+FHATHI                                      
         LH    RE,LSDSPROW                                                      
         LA    RE,1(RE)                                                         
         STH   RE,S.FHAD                                                        
*                                                                               
         MVC   S.FHDA(L'LCWORK1),BCSPACES                                       
         XR    RF,RF               PUT HEADERS INTO DATA FIELD                  
SHED02   ICM   RF,1,H.FHLN                                                      
         BZ    SHED10                                                           
         LH    RE,H.FHAD                                                        
         BCTR  RE,0                                                             
         LA    RE,S.FHDA(RE)                                                    
         SH    RF,=Y(FHDAD+1)                                                   
         EX    RF,*+4                                                           
         MVC   0(0,RE),H.FHDA                                                   
         LA    RF,FHDAD+1(RF)                                                   
         BXH   R3,RF,SHED02        BUMP R3 TO NEXT HEADER                       
*                                                                               
SHED10   IC    RF,S.FHLN           COPY SQUASHED FIELD INTO ORIGINAL            
         EX    RF,*+4                                                           
         MVC   0(0,R4),S.FHD                                                    
         AR    RF,R4                                                            
         ST    RF,4(R5)            SAVE A(NEW END OF HEADER)                    
         B     EXIT                                                             
         DROP  H,S                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST LINE                                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDLIN   LA    RE,BSLINE           CLEAR BSLINE                                 
         LA    RF,L'BSLINE                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,BSLINE                                                        
         USING FHD,R2                                                           
         XR    RE,RE                                                            
         ICM   RE,1,LSSUBLEN       TEST FOR SUB-ACTION FIELD                    
         BZ    BLDL04                                                           
         LA    RF,FHDAD+FHDAD(RE)                                               
         STC   RF,FHLN                                                          
         MVI   FHAT,FHATXH                                                      
         LH    RF,LSDSPROW                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,3,FHAD                                                        
         EX    RE,*+4                                                           
         MVC   FHDA(0),LCSPACES                                                 
         AR    R2,RE                                                            
         MVC   FHNU,LSSUBHLP       SET HELP PANEL NUMBER                        
         MVC   FHSC,LSLSTSCR       SET SCREEN NUMBER                            
         TM    LSSTAT1,LSSMAIN     MAINT LIST?                                  
         BZ    *+10                                                             
         MVC   FHSC,CSREC          SET SCREEN NUMBER FROM RECORD                
         LA    R2,FHDAD+FHDAD(R2)                                               
*                                                                               
BLDL04   LA    R4,LSLIN                                                         
         USING LINTABD,R4                                                       
*                                                                               
BLDL06   CLI   LINTABD,LINTEOT                                                  
         BE    BLDL14                                                           
         LH    RE,LINFLD#                                                       
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         A     RE,AFDRADDR                                                      
         L     R3,0(RE)                                                         
         USING FDRELD,R3                                                        
*                                                                               
         LH    RE,LINHCOL          SET FIELD COLUMN POSITION                    
         XR    RF,RF                                                            
         IC    RF,FDRLCDSP                                                      
         AR    RE,RF                                                            
         STH   RE,LINFCOL                                                       
*                                                                               
         TM    LININDS,LINIOPEN    TEST FOR OPEN INPUT FIELD                    
         BZ    BLDL08                                                           
         XR    RE,RE               YES - CREATE NEW FIELD                       
         IC    RE,FHLN                                                          
         AR    R2,RE                                                            
         MVC   FHAT,FDRFHAT                                                     
         OI    FHAT,FHATXH                                                      
         LH    R1,LINFCOL                                                       
         AH    R1,LSDSPROW                                                      
         STCM  R1,3,FHAD                                                        
*                                                                               
         LA    RF,FHD              SET DISPLACEMENT TO LIST HEADER              
         LA    RE,BSLINE                                                        
         SR    RF,RE                                                            
         STH   RF,LINHDR                                                        
         XR    RE,RE                                                            
         IC    RE,FDRLCLEN                                                      
         LA    RF,FHDAD+FHDAD(RE)                                               
         STC   RF,FHLN                                                          
         AR    R2,RE               SET UP EXTENDED FIELD HEADER                 
         MVC   FHNU,FDRHLP                                                      
         MVC   FHSC,LSLSTSCR                                                    
         TM    LSSTAT1,LSSMAIN     MAINT LIST?                                  
         BZ    *+10                                                             
         MVC   FHSC,CSREC          SET SCREEN NUMBER FROM RECORD                
         MVC   FHXA,FDRFHXA                                                     
         MVC   FHUS,LINFLD#                                                     
         LA    R2,FHDAD+FHDAD(R2)                                               
         XC    LINDSP,LINDSP                                                    
         B     BLDL12                                                           
*                                                                               
BLDL08   CLI   FHLN,0              UNLESS ALREADY DONE                          
         BNE   BLDL10              SET UP PROTECTED FIELD HEADER                
*                                                                               
         MVI   FHAT,FHATPR+FHATLC                                               
         LH    RE,LINFCOL                                                       
         AH    RE,LSDSPROW                                                      
         STCM  RE,3,FHAD                                                        
*                                                                               
BLDL10   LH    RF,LINFCOL                                                       
         AH    RF,LSDSPROW                                                      
         XR    RE,RE                                                            
         ICM   RE,3,FHAD                                                        
         SR    RF,RE                                                            
         STH   RF,LINDSP           SET DISPLACEMENT WITHIN FIELD                
         IC    RE,FDRLCLEN                                                      
         LA    RF,FHDAD(RE,RF)                                                  
         STC   RF,FHLN             SET NEW FIELD LENGTH                         
         LA    RF,FHD              SET DISPLACEMENT TO LIST HEADER              
         LA    RE,BSLINE                                                        
         SR    RF,RE                                                            
         STH   RF,LINHDR                                                        
*                                                                               
BLDL12   LA    R4,LINTABL(R4)      BUMP R4                                      
         B     BLDL06                                                           
*                                                                               
BLDL14   XR    RE,RE               BUMP R2 TO END-OF-LINE                       
         ICM   RE,1,FHLN                                                        
         BZ    *+6                                                              
         AR    R2,RE                                                            
*                                                                               
         OC    LS1ROWLN,LS1ROWLN   SET LENGTH OF 1 MULTIPLE ROW?                
         BNZ   BLDL16              YES                                          
*                                                                               
         LA    RE,BSLINE                                                        
         XR    RF,RF                                                            
         CLI   LSSUBLEN,0                                                       
         BE    *+8                                                              
         IC    RF,FHLN-FHD(RE)     GO PAST SUB-ACTION FIELD                     
         AR    RE,RF                                                            
         LR    RF,R2               CURRENT DISPLACEMENT                         
         SR    RF,RE                                                            
         STH   RF,LS1ROWLN         SAVE LENGTH OF 1 ROW                         
*                                                                               
BLDL16   LA    RE,BSLINE                                                        
         SR    R2,RE                                                            
         STH   R2,LSLINLEN         SAVE LENGTH OF LIST LINE                     
         DROP  R2,R3                                                            
*                                                                               
         TM    LSSTAT1,LSSMULIN    MULTIPLE RECORDS PER LINE?                   
         BZ    BLDLINX             NO                                           
         GOTOX ABLDMUL,BSLINE      BUILD MULTIPLE COPIES OF THIS LINE           
*                                                                               
BLDLINX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD MULTIPLE COPIES OF LINE                            *         
*                                                                     *         
* NTRY: R1 = A(LINE)                                                  *         
*       LSLINROW = NUMBER OF COPIES                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
F        USING FHD,R1              R1=A(COPY FROM LINE)                         
T        USING FHD,R2              R2=A(COPY TO LINE)                           
BLDMUL   LR    R2,R1                                                            
         XR    RF,RF                                                            
         ICM   RF,1,T.FHLN                                                      
         BZ    *+8                                                              
         BXH   R2,RF,*-8           FIND END OF LINE                             
*                                                                               
         LR    RF,R2                                                            
         SR    RF,R1                                                            
         MH    RF,LSLINROW                                                      
         AR    RF,R1                                                            
         XR    RE,RE                                                            
         BCTR  RF,0                RF=A(END OF LINE -1)                         
*                                                                               
BMUL02   IC    RE,F.FHLN           COPY FIELDS                                  
         EX    RE,*+4                                                           
         MVC   T.FHD(0),F.FHD                                                   
*                                                                               
         ICM   R3,3,T.FHAD                                                      
         AH    R3,LSCOLLIN         ADJUST SCREEN ADDRESS                        
         LA    R3,1(R3)                                                         
         STCM  R3,3,T.FHAD                                                      
         AR    R1,RE                                                            
         BXLE  R2,RE,BMUL02                                                     
*                                                                               
         MVI   T.FHD,0             MAKE SURE END AT END OF LINE                 
BLDMULX  B     EXIT                                                             
         DROP  F,T                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SAVE BOTTOM OF SCREEN INTO BSBOT                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SAVBOT   L     RE,ATWA                                                          
         AH    RE,LSBOTDSP                                                      
         LH    RF,LSBOTSIZ                                                      
         LA    R0,BSBOT                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
SAVBOTX  B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO RESTORE BOTTOM OF SCREEN                                 *         
*                                                                     *         
* NTRY: P1 = A(TWA FOR BOTTOM OF SCREEN)                              *         
* EXIT: P2 = A(END OF TWA)                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RESBOT   L     RE,0(R1)                                                         
         LR    RF,RE                                                            
         S     RF,ATWA                                                          
         LR    R0,RF                                                            
         SH    R0,LSBOTDSP         INDEX FACTOR                                 
*                                                                               
         STH   RF,LSBOTDSP         SET DISPLACEMENT TO BOTTOM OF LIST           
         LH    RF,LSTBTDSP                                                      
         AR    RF,R0                                                            
         STH   RF,LSTBTDSP                                                      
         LH    RF,LSFOODSP                                                      
         AR    RF,R0                                                            
         STH   RF,LSFOODSP                                                      
*                                                                               
         LH    RF,LSBOTSIZ         COPY BOTTOM OF SCREEN                        
         LA    R2,BSBOT                                                         
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
*                                                                               
RESBOTX  ST    RE,0(R1)            SAVE END OF TWA                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD ROW                                                *         
*                                                                     *         
* NTRY: P1=A(TO DATA SCREEN ADDRESS)                                  *         
*       P2=A(TO LINE)                                                 *         
*       P3=A(FROM LINE)                                               *         
* EXIT: P1=A(NEXT LINE DATA SCREEN ADDRESS)                           *         
*       P2=A(NEXT TO LINE)                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDROW   LM    R2,R4,0(R1)                                                      
T        USING FHD,R3              R3=A(TO LINE)                                
F        USING FHD,R4              R4=A(FROM LINE)                              
*                                                                               
         XR    RF,RF                                                            
BROW02   ICM   RF,1,F.FHLN                                                      
         BZ    BLDROWX                                                          
         EX    RF,*+4                                                           
         MVC   T.FHD(0),F.FHD                                                   
*                                                                               
         ICM   RE,3,F.FHAD                                                      
         AR    RE,R2                                                            
         STCM  RE,3,T.FHAD                                                      
         AR    R3,RF                                                            
         BXH   R4,RF,BROW02                                                     
*                                                                               
BLDROWX  AH    R2,LSCOLROW                                                      
         STM   R2,R3,0(R1)                                                      
         B     EXIT                                                             
         DROP  T,F                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ENTRY TO LINE TABLE                                  *         
*                                                                     *         
* NTRY: R1=A(DCTABD ENTRY)                                            *         
*       ALPARM SET UP                                                 *         
* EXIT: CC=NOT EQUAL IF END OF LINE HAS BEEN REACHED                  *         
*       ALPARM UPDATED                                                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
ADDLIN   LR    R3,R1                                                            
         USING DCTABD,R3                                                        
         L     R4,ALALIN                                                        
         USING LINTABD,R4          R4=A(LINE TABLE ENTRY)                       
*                                                                               
         OC    ALCURCOL,ALCURCOL   ADDING FIRST ENTRY?                          
         BNZ   ADLN02              NO                                           
         LA    R4,LSLIN                                                         
         MVC   ALCURCOL,=AL2(1)    SET COLUMN 1 POSITION                        
         XR    RE,RE                                                            
         ICM   RE,1,LSSUBLEN       SUB-ACTION FIELD ON SCREEN?                  
         BZ    ADLN02              NO                                           
         LA    RE,2(RE)                                                         
         STH   RE,ALCURCOL         COLUMN 1 = (L'SUB-ACT FIELD + 2)             
         LA    RE,FHDAD+FHDAD-2                                                 
         STH   RE,ALCURLEN         SET MEMORY USED FOR SUB-ACTION FIELD         
*                                                                               
ADLN02   LA    R0,LSLINX           END OF LINE TABLE                            
         CR    R4,R0               FILLED LINE TABLE?                           
         BNL   EXITL               YES                                          
*                                                                               
         XC    LINTABD(LINTABL),LINTABD  CLEAR NEXT LINE TABLE ENTRY            
*                                                                               
         MVI   GCFULL1,FDRELQ      GET FDREL FOR THIS COLUMN                    
         MVC   GCFULL1+1(2),DCTFLD#                                             
         GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'N',GCFULL1)                         
         L     R5,0(R1)            A(FDREL) RETURNED                            
         USING FDRELD,R5                                                        
*                                                                               
         XR    R0,R0               FLAG NEW ROW REQUEST UNPROCESSED             
ADLN04   XR    RE,RE                                                            
         IC    RE,FDRLHLEN         WIDTH OF NEXT COLUMN                         
         LH    RF,ALCURCOL         RF=LENGTH OF LINE USED                       
         LA    R2,0(RE,RF)                                                      
         CH    R2,LSCOLLIN         COLUMN FITS ON LINE?                         
         BH    ADLNL               NO                                           
*                                                                               
         TM    LSSTAT1,LSSMUROW    MULTIPLE ROWS PER LIST LINE?                 
         BZ    ADLN12              NO                                           
*                                                                               
         XR    R1,R1                                                            
ADLN06   LA    R1,COLS#Q(R1)       ADD NUMBER OF COLUMNS IN A ROW               
         CR    R1,RF               COLUMN STARTS ON CURRENT ROW?                
         BL    ADLN06              NO                                           
*                                                                               
         TM    DCTINDS1,DCTINEWL   NEW ROW REQUEST?                             
         BZ    ADLN08              NO                                           
         LTR   R0,R0               PROCESSED NEW ROW REQUEST ONCE?              
         BNZ   ADLN08              YES                                          
         LA    R0,1                FLAG PROCESSED NOW                           
         B     ADLN10              YES                                          
*                                                                               
ADLN08   CR    R2,R1               COLUMN STARTS NEW ROW?                       
         BL    ADLN12              NO - CONTINUE                                
*                                                                               
ADLN10   AH    R1,ALCOL2ND         YES - SET NEW ROW START                      
         STH   R1,ALCURCOL                                                      
         B     ADLN04              TRY AGAIN                                    
*                                                                               
ADLN12   LA    R2,1(R2)            R2=NEXT COLUMN POSITION                      
         TM    LSSTAT1,LSSMUROW    TEST MULTIPLE ROWS PER LINE                  
         BZ    ADLN14                                                           
         TM    ALINDS,ALIVAR       TEST VARIABLE COLUMN                         
         BZ    ADLN14                                                           
         OC    ALCOL2ND,ALCOL2ND   TEST START OF VAR. COLS SET                  
         BNZ   ADLN14                                                           
         STH   RF,ALCOL2ND         SET START OF VARIABLE COLUMNS                
*                                                                               
ADLN14   XC    LINFLD#,LINFLD#                                                  
         MVC   LINFLD#+1(1),GS#FDR                                              
         MVC   LINHCOL,ALCURCOL    SET COLUMN FOR THIS HEADING                  
         XR    RE,RE                                                            
         IC    RE,FDRLHLEN                                                      
         STH   RE,LINHWDTH         SET WIDTH OF COLUMN                          
*                                                                               
         TM    DCTINDS1,DCTIOPEN   OPEN COLUMN?                                 
         BZ    *+8                 NO                                           
         OI    LININDS,LINIOPEN    SET OPEN COLUMN                              
         TM    ALINDS,ALIVAR       VARIABLE COLUMN?                             
         BZ    *+8                 NO                                           
         OI    LININDS,LINIVAR     SET VARIABLE COLUMN                          
*                                                                               
         LH    RF,ALCURLEN         RF=CURRENT LENGTH IN TWA OF LINE             
         XR    RE,RE                                                            
         IC    RE,FDRLCLEN         WIDTH OF INPUT FIELD IN COLUMN               
         LA    RF,0(RE,RF)         ADD WIDTH OF INPUT FIELD                     
*                                                                               
         TM    LININDS,LINIOPEN    INPUTTABLE FIELD?                            
         BZ    ADLN16              NO                                           
         LA    RF,FHDAD+FHDAD(RF)  ADD HEADER LENGTHS                           
         OI    ALINDS,ALINEWF      SET NEW FIELD REQUIRED NEXT TIME             
         B     ADLN20                                                           
*                                                                               
ADLN16   TM    ALINDS,ALINEWF      NEW FIELD REQUIRED?                          
         BZ    ADLN18              NO                                           
         LA    RF,FHDAD(RF)        ADD FIELD HEADER LENGTH                      
         NI    ALINDS,FF-ALINEWF   SET SAME FIELD NEXT TIME                     
         B     ADLN20                                                           
*                                                                               
ADLN18   LA    RF,1(RF)            ADD 1 SPACE BETWEEN COLUMNS                  
         NI    ALINDS,FF-ALINEWF   SET SAME FIELD NEXT TIME                     
*                                                                               
ADLN20   CH    RF,LSLINMAX         LENGTH STILL WITHIN LIMIT                    
         BH    ADLNL               NO                                           
         STH   RF,ALCURLEN                                                      
*                                                                               
ADLNOK   STH   R2,ALCURCOL                                                      
         LA    R4,LINTABL(R4)                                                   
         MVI   LINTABD,LINTEOT                                                  
         ST    R4,ALALIN                                                        
         B     EXITOK                                                           
*                                                                               
ADLNL    XC    LINTABD(LINTABL),LINTABD                                         
         MVI   LINTABD,LINTEOT                                                  
         B     EXITL                                                            
         DROP  R3,R4,R5                                                         
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DELETE LAST LINE TABLE ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
DELLIN   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         CLI   BSWAY,BSWLEFT       TEST BUILDING LEFTWARDS                      
         BE    DELLIN02                                                         
         LH    RF,LSVARRHS         NO CHANGE R-H-S                              
         BCTR  RF,0                                                             
         STH   RF,LSVARRHS                                                      
         B     DELLIN10                                                         
*                                                                               
DELLIN02 LH    RF,LSVARLHS                                                      
         LA    RF,1(RF)                                                         
         STH   RF,LSVARLHS                                                      
*                                                                               
         B     DELLIN10                                                         
         LH    RF,LSLINRHS         YES - REMOVE FIRST COLUMN                    
         LA    RF,LSLIN(RF)                                                     
         USING LINTABD,RF                                                       
         LH    R0,LINHCOL                                                       
         SH    RF,=Y(LINTABL)                                                   
         SH    R0,LINHCOL          R0=SHIFT TO THE LEFT                         
         LH    RF,LSLINLHS                                                      
         LA    RF,LSLIN(RF)                                                     
DELLIN06 LH    RE,LINHCOL                                                       
         SR    RE,R0                                                            
         STH   RE,LINHCOL                                                       
         LA    RF,LINTABL(RF)                                                   
         CLI   LINTABD,LINTEOT                                                  
         BNE   DELLIN06                                                         
         DROP  RF                                                               
*                                                                               
DELLIN10 LH    RF,LSLINRHS         REMOVE LAST COLUMN                           
         LA    RF,LSLIN(RF)                                                     
         XC    0(LINTABL,RF),0(RF)                                              
         MVI   0(RF),LINTEOT                                                    
         SH    RF,=Y(LINTABL)                                                   
         LA    R0,LSLIN                                                         
         SR    RF,R0                                                            
         STH   RF,LSLINRHS         SET NEW RIGHT-HAND-SIDE                      
*                                                                               
DELLINX  LH    RF,LSVARPAG                                                      
         BCTR  RF,0                                                             
         STH   RF,LSVARPAG                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DRAW A BORDER AROUND THE LIST                            *         
*                                                                     *         
* NTRY: P1 = A(POINT IN TWA)                                          *         
* EXIT: P1 = A(NEW END IN TWA)                                        *         
***********************************************************************         
         SPACE 1                                                                
BORDER   DS    0H                                                               
         USING *,R7                                                             
*                                                                               
         L     R3,0(R1)                                                         
         USING FHD,R3                                                           
*                                                                               
         MVC   FHD(L'BDRTOP),BDRTOP                                             
         LH    R2,LSBDRTOP                                                      
         LA    RF,1(R2)                                                         
         STH   RF,FHAD                                                          
         LA    R3,L'BDRTOP(R3)                                                  
*                                                                               
BORDER02 MVC   FHD(L'BDRSIDE),BDRSIDE                                           
         LA    RF,COLS#Q-1(R2)                                                  
         STH   RF,FHAD                                                          
         LA    R3,L'BDRSIDE(R3)                                                 
         LA    R2,COLS#Q(R2)                                                    
         CH    R2,LSBDRBOT                                                      
         BL    BORDER02                                                         
*                                                                               
         MVC   FHD(L'BDRTOP),BDRTOP                                             
         LH    R2,LSBDRBOT                                                      
         LA    RF,3(R2)                                                         
         STH   RF,FHAD                                                          
         LA    R3,L'BDRTOP(R3)                                                  
*                                                                               
BORDERX  ST    R3,0(R1)                                                         
         B     EXIT                                                             
*                                                                               
BDRSIDE  DS    0XL11                                                            
         DC    AL1(L'BDRSIDE,FHATPR+FHATHI,0,0,0,0,0,0)                         
         DC    C'* *'                                                           
*                                                                               
BDRTOP   DS    0XL85                                                            
         DC    AL1(L'BDRTOP,FHATPR+FHATHI,0,0,0,0,0,0)                          
         DC    39C'* '                                                          
         EJECT                                                                  
*&&NOP                                                                          
***********************************************************************         
* ROUTINE TO DRAW A BORDER AROUND THE LIST                            *         
*                                                                     *         
* NTRY: P1 = A(POINT IN TWA)                                          *         
* EXIT: P1 = A(NEW END IN TWA)                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BORDER   L     R3,0(R1)                                                         
         USING FHD,R3                                                           
         XC    FHD(FHDAD),FHD      CLEAR HEADER                                 
*                                                                               
         LH    RF,LSCOLLIN         WIDTH OF THE LIST                            
         LA    RF,FHDAD(RF)                                                     
         STC   RF,FHLN             SET LENGTH OF FIELD                          
         MVI   FHAT,FHATPR+FHATHI  SET PROTECTED/HIGHLIGHTED                    
         MVI   FHOI,FHOITR         TRANSMIT IT                                  
         LH    RF,LSCOLLIN         WIDTH OF A COLUMN                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FHDA(0),BDRTOP      MOVE IN BORDER CHARACTERS                    
         LA    RF,1(RF)                                                         
         STC   RF,FHIL             SET INPUT LENGTH                             
         LH    R2,LSBDRTOP                                                      
         LA    R2,1(R2)                                                         
         STH   R2,FHAD             SET ADDRESS - SAVE R2 FOR SIDES              
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    R3,0(RF,R3)         NEXT FIELD ON SCREEN                         
*                                                                               
BORDER02 MVC   FHD(L'BDRSIDE),BDRSIDE  LHS BORDER                               
         LA    RF,COLS#Q(R2)       R2 POINTS TO COLUMN 2 IN LINE ABOVE          
         STCM  RF,3,FHAD           A(COLUMN 2 IN THIS LINE)                     
*                                                                               
         LA    R3,L'BDRSIDE(R3)                                                 
         MVC   FHD(L'BDRSIDE),BDRSIDE  RHS BORDER                               
         AH    RF,LSCOLLIN                                                      
         LA    RF,5(RF)                                                         
         STCM  RF,3,FHAD           A(END OF THIS LIST LINE)                     
*                                                                               
         LA    R2,COLS#Q(R2)       NEXT LINE ON SCREEN                          
         CH    R2,LSBDRBOT         BUILT ALL SCREEN LINES YET?                  
         BL    BORDER02            NO                                           
*                                                                               
         XC    FHD(FHDAD),FHD      CLEAR HEADER                                 
         LH    RF,LSCOLLIN         WIDTH OF THE LIST                            
         LA    RF,FHDAD(RF)                                                     
         STC   RF,FHLN             SET LENGTH OF FIELD                          
         MVI   FHAT,FHATPR+FHATHI  SET PROTECTED/HIGHLIGHTED                    
         MVI   FHOI,FHOITR                                                      
         LH    RF,LSCOLLIN         WIDTH OF A COLUMN                            
         STC   RF,FHIL             SET INPUT LENGTH                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FHDA(0),BDRTOP      MOVE IN BORDER CHARACTERS                    
         LH    R2,LSBDRBOT                                                      
         LA    R2,1(R2)                                                         
         STH   R2,FHAD             SET FIELD ADDRESS                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    R3,0(RF,R3)         NEXT FIELD ON SCREEN                         
*                                                                               
BORDERX  ST    R3,0(R1)                                                         
         B     EXIT                                                             
*                                                                               
BDRSIDE  DS    0XL9                                                             
         DC    AL1(L'BDRSIDE,FHATPR+FHATHI,0,0,0,1,FHOITR,0)                    
         DC    C'*'                                                             
*                                                                               
BDRTOP   DS    0XL78                                                            
         DC    39C'* '                                                          
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ROUTINE TO GET NEXT RECORD IN LIST                                  *         
*                                                                     *         
* NTRY: R3=LGETFRST / LGETNEXT AS REQUIRED                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
GETNXT   L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
*                                                                               
GNXT02   GOTOX AOLY,RTPARM,OLIST,(R3),LCRECKEY,LSLASKEY,LCEQUREC                
         BNE   EXITL               ASK OVERLAY FOR NEXT IN LIST                 
*                                                                               
GNXT04   TM    LSSTAT1,LSSMAIN     MAINTENANCE TYPE LIST?                       
         BO    GNXT06              YES - DON'T FILTER THEN                      
         CLI   CSREC,O#FLTR        FILTER OVERLAY?                              
         BE    GNXT06              YES - INVALID THEN                           
*                                                                               
         GOTOX AGEN,RTPARM,OFILT,FDOD,LCRECKEY                                  
         BE    GNXT06              PASSED DIRECTORY FILTER                      
         LA    R3,LGETNEXT                                                      
         B     GNXT02                                                           
*                                                                               
GNXT06   GOTOX ALST,RTPARM,OLIST,LTSARDIR,LCRECKEY                              
*                                                                               
         GOTOX AGETREC,0           GET RECORD                                   
*                                                                               
         TM    LSSTAT1,LSSMAIN     MAINTENANCE TYPE LIST?                       
         BO    GNXT08              YES - DON'T FILTER THEN                      
         CLI   CSREC,O#FLTR        FILTER OVERLAY?                              
         BE    GNXT08              YES - NO OTHER FILTERS                       
*                                                                               
         GOTOX AGEN,RTPARM,OFILT,FDOR,LCAREC                                    
         BE    GNXT08              PASSED RECORD FILTERING                      
         LA    R3,LGETNEXT                                                      
         B     GNXT02                                                           
*                                                                               
GNXT08   GOTOX ALST,RTPARM,OLIST,LTSARFIL,LCAREC                                
*                                                                               
         CLI   CSREC,O#FLTR        FILTER OVERLAY?                              
         BE    GNXTX               YES - NO OTHER FILTERS                       
*                                                                               
         GOTOX ALST,RTPARM,OLIST,LTSARTSA,0                                     
         BE    GNXT10              PASSED USER FILTERING                        
         LA    R3,LGETNEXT                                                      
         B     GNXT02                                                           
*                                                                               
GNXT10   GOTOX AGEN,RTPARM,OFILT,FDOT,ATLST                                     
         BE    GNXTX               PASSED TSAR FILTERING                        
         LA    R3,LGETNEXT                                                      
         B     GNXT02                                                           
*                                                                               
GNXTX    MVC   LSLASKEY,LCRECKEY   SAVE LAST KEY                                
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
LCSPACES DC    256C' '                                                          
EOS      DC    X'0001010000008000' END-OF-SCREEN DATA                           
CORETAB  DC    C'CORETAB'                                                       
         DS    0H                                                               
         DS    (LTORGX-*)X                                                      
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
LCWORKD  DSECT                                                                  
LCRELO   DS    A                                                                
LCAR1    DS    A                   A(INCOMING PARAMETER LIST)                   
RTPARMS  DS    0XL24               SAVED PARAMETERS                             
RTPARMS1 DS    A                                                                
         ORG   *-1                                                              
LCOBJECT DS    XL1                 OBJECT                                       
RTPARMS2 DS    A                                                                
         ORG   RTPARMS2                                                         
LCSVB    DS    XL1                 SUB-VERB                                     
         DS    XL2                                                              
LCVERB   DS    XL1                 VERB                                         
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
*                                                                               
RTPARM   DS    XL24                                                             
LCMIF    DS    0XL256                                                           
LCMIFL   DS    XL2                 L'LCMIFL+L'LCMIFS                            
LCMIFS   DS    XL254               MIFELS LIST                                  
LCMIFELD DS    XL80                NEW MIFELD                                   
LCSAVFLD DS    XL80                SAVED FIELD                                  
*                                                                               
LCROUTS  DS    0A                  * LIST CONTROLLER ROUTINES *                 
ALST     DS    A                                                                
AGETREC  DS    A                                                                
ACONT    DS    A                                                                
ALSTMSG  DS    A                                                                
ASCRDOWN DS    A                                                                
ASCRUP   DS    A                                                                
ASCRHORZ DS    A                                                                
ACURLIN  DS    A                                                                
ASETPAG  DS    A                                                                
ADISPAG  DS    A                                                                
AVALPAG  DS    A                                                                
AVALNPAG DS    A                                                                
ADISLINE DS    A                                                                
ADISTOT  DS    A                                                                
AVALLINE DS    A                                                                
AVALNLIN DS    A                                                                
AFNDMIF  DS    A                                                                
ATSTMIF  DS    A                                                                
AADDMIF  DS    A                                                                
AVALFLD  DS    A                                                                
AREMLINE DS    A                                                                
ACLRLINE DS    A                                                                
ATSTLINE DS    A                                                                
ASCRHAMT DS    A                                                                
ASCRVAMT DS    A                                                                
AGETITEM DS    A                                                                
ANEWITEM DS    A                                                                
ANXTSCRN DS    A                                                                
APUTITEM DS    A                                                                
ASAVITEM DS    A                                                                
ASHUFFLE DS    A                                                                
AINISCR  DS    A                                                                
ABLDTOP  DS    A                                                                
ABLDBOT  DS    A                                                                
ABLDLST  DS    A                                                                
ABLDSCR  DS    A                                                                
ABLDFIX  DS    A                                                                
ABLDVAR  DS    A                                                                
ABLDREV  DS    A                                                                
ABLDSPC  DS    A                                                                
ASPCOUT  DS    A                                                                
ABLDHED  DS    A                                                                
ABLDLIN  DS    A                                                                
ABLDMUL  DS    A                                                                
ASAVBOT  DS    A                                                                
ARESBOT  DS    A                                                                
ABLDROW  DS    A                                                                
AADDLIN  DS    A                                                                
ADELLIN  DS    A                                                                
ABORDER  DS    A                                                                
AGETNXT  DS    A                                                                
LCROUTSN EQU   (*-LCROUTS)/L'LCROUTS                                            
*                                                                               
LCAREC   DS    A                   A(IO AREA FOR CURRENT LIST RECORD)           
LCEQUREC DS    A                   EQUATE FOR IO AREA                           
*                                                                               
LCRECKEY DS    XL64                MAX LENGTH OF DIRECTORY RECORD               
LCHORSCR DS    H                   HORIZONTAL SCROLL AMOUNT                     
LCVERSCR DS    H                   VERTICAL SCROLL AMOUNT                       
LCROWS   DS    H                   ROWS ON THIS TSAR RECORD                     
LCWORK1  DS    CL92                80+8+8                                       
         ORG   LCWORK1                                                          
UCSCR    DS    0X                                                               
UC@PAGE  DS    CL8                                                              
UC@MAX   DS    CL8                                                              
UC@HALF  DS    CL8                                                              
LCSCR    DS    0X                                                               
LC@PAGE  DS    CL8                                                              
LC@MAX   DS    CL8                                                              
LC@HALF  DS    CL8                                                              
         ORG                                                                    
LCWORK2  DS    CL92                80+8+8                                       
LCDUB    DS    D                                                                
LCHALF   DS    H                                                                
LCHALF1  DS    H                                                                
LCBYTE   DS    XL1                                                              
*                                                                               
*                                  * BLDLST ROUTINE FIELDS *                    
BLLST#X  DS    XL2                 HIGH RECORD REQUIRED                         
*                                                                               
*                                  * BLDSCR ROUTINE FIELDS *                    
BSWAY    DS    XL1                 DIRECTION SCREEN TO BE BUILD                 
BSWRIGHT EQU   C'R'                RIGHTWARDS FROM LSVARLHS                     
BSWLEFT  EQU   C'L'                LEFTWARDS FROM LSVARRHS                      
*                                                                               
BSLIN    DS    XL(LSLINS)          SAVED LINE TABLE                             
*                                                                               
BSHEAD1  DS    XL512               HEADLINE 1                                   
BSHEAD2  DS    XL512               HEADLINE 2                                   
*                                                                               
BSLINE   DS    XL1024              LIST LINE                                    
*                                                                               
BSFOOT   DS    XL100               FOOT LINE                                    
BSBOT    DS    XL512               BOTTOM OF SCREEN                             
*                                                                               
         DS    0A                                                               
ALPARM   DS    0XL12               * ADDLIN ROUTINE FIELDS *                    
ALALIN   DS    A                   A(LINE TABLE ENTRY)                          
ALCURLEN DS    H                   CURRENT MEMORY UESD FOR LINE                 
ALCURCOL DS    H                   CURRENT COLUMN POSITION                      
ALCOL2ND DS    H                   COLUMN START OF SECOND ROW                   
ALINDS   DS    XL1                 INDICATOR BYTE FOR COLUMNS                   
ALINEWF  EQU   X'80'               NEW FIELD REQUIRED FOR COLUMN                
ALIVAR   EQU   X'40'               ADDING VARIABLE COLUMNS                      
         DS    XL1                 N/D                                          
         ORG   ALPARM+L'ALPARM                                                  
*                                                                               
SVLST    DS    XL(L'TLST)                                                       
*                                                                               
LCWORKL  EQU   *-LCWORKD                                                        
         EJECT                                                                  
* GEFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
GEFIL03  CSECT                                                                  
         ORG                                                                    
         ORG   GEFIL03+(((*-GEFIL03)/2048)+1)*2048                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031GEFIL03S  05/22/00'                                      
         END                                                                    
