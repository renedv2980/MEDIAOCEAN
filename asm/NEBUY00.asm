*          DATA SET NEBUY00    AT LEVEL 004 AS OF 02/26/20                      
*PHASE T31100B,+0                                                               
*INCLUDE NETBLRDR                                                               
*INCLUDE SPFMTINO                                                               
*INCLUDE CALCASHP                                                               
         TITLE 'NETPAK BUY PROGRAM BASE - T31100'                               
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 004 09DEC19 <SPEC-39994> ALLOW UNITS TO USE THE COS2 FACTOR    *         
*                               FROM THE PACKAGE                      *         
***********************************************************************         
T31100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 BUYWRKX-BUYWRKD,**BUY00,RA,RR=RE,CLEAR=YES                       
         LA    R7,2048(RA)                                                      
         LA    R7,2048(R7)                                                      
         USING T31100+8192,R7       THIRD BASE REGISTER                         
         LR    R9,RC               R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         ST    RE,BRELO            SAVE BASE RELOCATION                         
         ST    RB,BASE1            SAVE BASE REGISTERS                          
         ST    RA,BASE2                                                         
         ST    R7,BASE3                                                         
         ST    RD,AWORK                                                         
*                                                                               
         ST    R1,APARM            PARAMETER LIST FROM MONITOR                  
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
BUY01    L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         MVI   DDS,NO                                                           
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,YES                                                          
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAUSRID                                                  
         MVC   AGYALPH,TWAAGY                                                   
*                                                                               
BUY02    L     RE,ACOMFACS         OBTAIN COMFACS MODULE ADDRESSES              
         USING COMFACSD,RE                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VDEMOUT,CDEMOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGLOBBER,CGLOBBER                                                
         DROP  RE                                                               
*                                                                               
*                                                                               
BUY03    LA    R2,CORETAB          OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,VCALLOV                                                       
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
BUY04    CLI   0(R2),0                                                          
         BE    BUY05                                                            
         MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
BUY05    LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,BUY04                                                         
*                                                                               
         MVI   DMCB+7,QMSPACK      GET A(MSPACK)                                
         GOTO1 (RF),(R1),0                                                      
         MVC   DUB(4),DMCB         FULL2=A(MSPACK)                              
*                                                                               
         MVI   DMCB+7,QMSUNPK      GET A(MSUNPK)                                
         GOTO1 (RF),(R1),0                                                      
         MVC   DUB+4(4),DMCB       FULL2=A(MSUNPK)                              
*                                                                               
BUY06    LAY   R1,BASETAB          SET UP BASE FACILITIES                       
         LA    R0,BASETABC         COUNTER                                      
         LA    RE,BASEFACS         POINTER TO WORKING STORAGE                   
BUY07    L     RF,0(R1)            ADDRESS OF BASE FACILITY                     
         A     RF,BRELO            RELOCATE IT                                  
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)            NEXT ADDRESS                                 
         LA    RE,4(RE)            NEXT OUTPUT AREA                             
         BCT   R0,BUY07                                                         
*                                                                               
         MVC   VMSPACK,DUB         SET A(MSPACK)                                
         MVC   VMSUNPK,DUB+4       SET A(MSPACK)                                
         L     RF,=V(NETBLRDR)     SET A(NETBLRDR)                              
         A     RF,BRELO                                                         
         ST    RF,VBILLRDR                                                      
         L     RF,=V(CALCASHP)     SET A(CALCASHP)                              
         A     RF,BRELO                                                         
         ST    RF,VCALCASH                                                      
*                                                                               
BUY10    LA    R1,COMMIN           LINKAGE TO COMMON ROUTINES                   
         SR    R2,R2                                                            
         LA    R3,BASECOM          OUTPUT ADDRESSES                             
         LA    R0,COMMONS          COUNTER                                      
*                                                                               
BUY11    ST    R1,0(R3)                                                         
         STC   R2,0(R3)                                                         
         LA    R2,4(R2)            BUMP BRANCH INDEX                            
         LA    R3,4(R3)            NEXT OUTPUT AREA                             
         BCT   R0,BUY11                                                         
         SPACE 1                                                                
*                                                                               
BUY12    LA    RE,IOAREA1          SET ADCONS FOR EXTENDED ADDRESSING           
         LA    R0,4                COUNTER - 4 I/O AREAS                        
         LA    R1,AIOAREA1         OUTPUT AREA                                  
         ST    RE,0(R1)                                                         
         LA    RE,IOLEN1(RE)                                                    
         LA    RE,IOLEN1(RE)                                                    
         LA    R1,4(R1)                                                         
         BCT   R0,*-16                                                          
         L     R1,=AL4(PROGREC-BUYWRKD)                                         
         LA    R1,BUYWRKD(R1)      SET ADCON FOR PROGRAM RECORD                 
         ST    R1,APROGREC                                                      
         L     R1,=AL4(OVWORK-BUYWRKD)                                          
         LA    R1,BUYWRKD(R1)      SET ADCON TO OVERLAY LOCAL STORAGE           
         ST    R1,AOVWORK                                                       
         MVI   SPACES,C' '         SET OTHER WORKING STORAGE VALUES             
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAYC)                                    
         MVC   XTRA,SPACES                                                      
         MVI   DEMFRST,YES         SET FIRST TIME DEMO LOOKUP SWITCH            
         MVC   BUYMSG,SPACES                                                    
         OI    BUYMSGH+6,X'80'                                                  
*                                                                               
         L     R5,ATIA             R5 POINTS TO BUYVALS                         
         USING BUYVALD,R5                                                       
         ST    R5,ABUYVALS                                                      
         LR    RE,R5               CLEAR BUY VALUES AREA                        
         LA    RF,PAGELEN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
         L     R1,=AL4(NEBLKXTD-BUYWRKD)                                        
         LA    R1,BUYWRKD(R1)      SET ADCON FOR EXTEND BLOCK                   
         ST    R1,NBEXTEND         A(EXTEND BLOCK)                              
         USING NBXTNDD,R1                                                       
         LA    RF,PACKREC                                                       
         AHI   RF,500                                                           
         ST    RF,NBXCDNL          A(COMSCORE DEMO NAME LIST)                   
         DROP  R1                                                               
         EJECT                                                                  
* ROUTINE TO TEST WHETHER WE'VE BEEN CALLED FROM $MAD FOR UPLOAD                
*                                                                               
TESTUP   ICM   RF,15,VGLOBBER                                                   
         BZ    VALAGY                                                           
         GOTO1 (RF),DMCB,=C'GETD',WORK,24,GLVXCTL                               
         CLI   DMCB+8,GLEGNF                                                    
         BE    VALAGY                                                           
         CLI   DMCB+8,0         CAN'T HANDLE OTHER ERRORS                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                               DELETE 'OLD' TRANSFER ELEM                      
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,WORK                                                          
         USING GLVXFRSY,RE                                                      
         CLC   GLVXFRSY(6),=C'NETNNA'  TEST FROM NAVIGATOR                      
         BE    GLNAV                                                            
         DROP  RE                                                               
*                                                                               
TESTUP20 MVI   BUYACTH,72          LOOK UP UPLOAD                               
         MVI   BUYACTH+5,6                                                      
         MVC   BUYACT,SPACES                                                    
         MVC   BUYACT(6),=CL6'UPLOAD'                                           
         B     VALACT                                                           
*                                                                               
*  AREA HANDLES CALLS FROM NAVIGATOR                                            
*                                                                               
*  GET BUY INFO PASSED FROM NENAV06                                             
*                                                                               
*                                                                               
* SAVE NAVIGATOR AND NBUY SESSIONS FOR DIALOGUE CALL                            
GLNAV    TWAXC BUYCLIH,CLRINPUTLEN=Y                                            
         LA    RE,WORK                                                          
         USING GLVXFRSY,RE                                                      
         MVC   NAVSESSN(2),GLVXSESR    NAVIGATOR/BUY SESSIONS                   
         TM    GLVXFLG1,GLV1SIDE       DO WE ALREADY HAVE NBU SESSION           
         BO    GLNAV020                 YES EXIT                                
*  GET NBUY SESSION FROM UTL                                                    
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         ICM   RF,15,CSWITCH                                                    
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 (RF),DMCB                                                        
         L     R2,DMCB                                                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING UTLD,R2                                                          
         MVC   NAVSESSN+1(1),TSESSION                                           
         DROP  R2,RE                                                            
*                                                                               
GLNAV020 L     R3,AIOAREA4                                                      
         USING BUYDRFTD,R3                                                      
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,4,GLVBUY1                            
         CLI   DMCB+8,0         CAN'T HANDLE OTHER ERRORS                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* DO WSSVR CALL TO GET INFO FROM NAV SYSTEM                                     
         XC    WORK+4(56),WORK+4                                                
         LA    R2,WORK+4                                                        
         USING FAWSSVRD,R2                                                      
         MVC   FAWSTOKN,WORK    SENT FROM NAVIGATOR                             
         OI    FAWSACTN,FAWSURST                                                
         MVC   FAWSADR,AIOAREA4                                                 
         MVC   FAWSLEN,=H'2000'                                                 
         PRINT GEN                                                              
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),FAWSSVRD                                                    
         PRINT NOGEN                                                            
         CLI   FAWSRTN,FAWSROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    FAWSACTN,FAWSUDEL                                                
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),FAWSSVRD                                                    
         DROP  R2                                                               
*                                                                               
* READ BUY DATA RECORD FROM TEMPSTR                                             
***      XC    DMCB(24),DMCB                                                    
***      MVC   DMCB+8(4),WORK      PAGE/TERMINAL NUMBER                         
***      MVC   DMCB+20(2),=C'L='                                                
***      MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
***      GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R3),0                     
***      CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
***      BE    *+6                                                              
***      DC    H'0'                                                             
         MVC   STEWSEQ,RDRSEQ                                                   
* FILL CLIENT FIELD                                                             
         MVI   BUYCLIH,11                                                       
         MVC   BUYCLI,SPACES                                                    
         MVC   BUYCLI(3),RDRCLI                                                 
         LA    R2,BUYCLIH                                                       
         BRAS  RE,FINDLEN                                                       
* FILL ESTIMATE                                                                 
         MVI   BUYESTH,15                                                       
         OI    BUYESTH+4,X'08'      SET TO NUMERIC                              
         MVC   BUYEST,SPACES                                                    
         MVC   BUYEST(3),RDREST                                                 
         LA    R2,BUYESTH                                                       
         BRAS  RE,FINDLEN                                                       
* FILL NETWORK                                                                  
         MVI   BUYNETH,12                                                       
         OI    BUYNETH+4,X'04'      SET TO ALPHA                                
         MVC   BUYNET,SPACES                                                    
         MVC   BUYNET(4),RDRNET                                                 
         LA    R2,BUYNETH                                                       
         BRAS  RE,FINDLEN                                                       
* FILL PACKAGE                                                                  
         MVI   BUYPAKH,11                                                       
         OI    BUYPAKH+4,X'08'      SET TO NUMERIC                              
         MVC   BUYPAK,SPACES                                                    
         MVC   BUYPAK(3),RDRPACK                                                
         LA    R2,BUYPAKH                                                       
         BRAS  RE,FINDLEN                                                       
* FILL PROGRAM                                                                  
         MVI   BUYPRGH,14                                                       
         MVC   BUYPRG,SPACES                                                    
         MVC   BUYPRG(6),RDRPCODE                                               
         LA    R2,BUYPRGH                                                       
         BRAS  RE,FINDLEN                                                       
* FILL ACTION                                                                   
         BRAS  RE,SETACT                                                        
         LA    R2,BUYACTH                                                       
         BRAS  RE,FINDLEN                                                       
*                                                                               
         L     RE,AOVWORK                                                       
         L     RF,AIOAREA4                                                      
         USING BUYUPLDD,RF                                                      
         MVC   0(75,RE),RUPDEMS     PASS REQUESTED DEMOS TO OVERLAY             
         MVC   200(L'RUPCDEMS,RE),RUPCDEMS   PASS REQ COMSCORE DEMOS            
         DROP  RF                                                               
         CLC   RDRTYPE,=CL2'DR'     IF NOT DRAFT MODE                           
         BNE   GLNAV100             BYPASS DATE AND DEMS FILL                   
         MVC   0(75,RE),RDRDEMS     PASS REQUESTED DEMOS TO OVERLAY             
         MVC   75(104,RE),RDRDATES  PASS REQUESTED DATES TO OVERLAY             
         MVC   179(8,RE),RDRTIME    PASS OVERRIDE INFO TO OVERLAY               
         MVC   187(1,RE),RDRSTAT2   SEND 2ND STATUS BYTE                        
*                                                                               
*  SET UP RETURN GLOBBER CALL                                                   
*                                                                               
GLNAV100 XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING GLVXFRSY,RE                                                      
         MVC   GLVXFRSY,=C'NET'    FROM NET/BUY                                 
         MVC   GLVXFRPR,=C'NBU'                                                 
         MVC   GLVXTOSY,=C'NET'    TO   NET/NNA                                 
         MVC   GLVXTOPR,=C'NNA'                                                 
         MVC   GLVXSESR(2),NAVSESSN                                             
*                                                                               
*******  OI    GLVXFLG1,GLV1RETN+GLV1TWA0+GLV1RETG                              
         OI    GLVXFLG1,GLV1RETN+GLV1RETG                                       
         OI    GLVXFLG1,GLV1SIDR+GLV1SIDE                                       
         OI    GLVXFLG1,GLV1SEPS+GLV1SEPD                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,24,GLVXCTL                           
         B     VALAGY                                                           
         EJECT                                                                  
****************************************************************                
* VALIDATION CODE REGISTER CONVENTIONS                         *                
* R3 POINTS TO ACTION TABLE ENTRY  R4 POINTS TO RECORD         *                
* R5 POINTS TO BUY VALUES          R6 POINTS TO NETBLOCK       *                
****************************************************************                
         SPACE 2                                                                
* START BY GETTING AGENCY VALUES                                                
*                                                                               
VALAGY   MVC   NBSELAGY,AGYALPH                                                 
         MVI   NBSELMED,C'N'                                                    
         MVI   NBSELMOD,NBVALAGY   READ AGENCY RECORD                           
         MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         MVC   NBAUTH,TWAAUTH                                                   
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD      TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AGENCY,AGYALPH                                                   
         MVC   AGYMED,NBACTAM                                                   
         L     R4,NBAIO                                                         
         USING AGYHDRD,R4                                                       
         MVC   AGYPRO,AGYPROF      EXTRACT PROFILE                              
*                                                                               
         MVC   SVAGYFL2,AGYFLAG2                                                
*                                                                               
         NI    DEMFLAG,X'FF'-D2PRE                                              
         TM    AGYFLAG2,AGYFLAG2_2DP    AGENCY IS 2 DECIMAL PRECISION?          
         BZ    *+12                                                             
         OI    DEMFLAG,D2PRE                                                    
         B     VALAGY10                                                         
*                                                                               
         TM    AGYFLAG2,AGYFLAG2_BDP    USER DEFINED PRECISION?                 
         BZ    *+12                                                             
         OI    DEMFLAG,D2PRE                                                    
         B     VALAGY10                                                         
*                                                                               
* INITIALIZE SECRET                                                             
*                                                                               
VALAGY10 DS    0H                                                               
         L     RE,ATWA                                                          
         A     RE,=A(BASETWA)                                                   
         USING SVAREA,RE                                                        
*                                                                               
         SPACE 1                                                                
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    VALACT                                                           
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',SVSECRET),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     VALACT                                                           
         DROP  R4,RE                                                            
         EJECT                                                                  
* FIRST VALIDATE ACTION                                                         
*                                                                               
VALACT   L     R3,=A(ACTTAB)                                                    
         A     R3,BRELO                                                         
*        LA    R3,ACTTAB                                                        
         USING ACTTABD,R3                                                       
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FTERM,FTERM                                                      
         MVI   FTERM,C','                                                       
         MVI   FERN,MISERR                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    VALACTR                                                          
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,L'ACTCODE                                                 
         BH    VALACTR             LONGER THAN A VALID ACTION CODE              
         CLI   FLDH+5,1                                                         
         BNE   VALACT2                                                          
         CLI   FLD,C'?'            TEST FOR QUESTION MARK                       
         BNE   VALACT2                                                          
         L     R3,=A(ACTTAB)                                                    
         A     R3,BRELO                                                         
*        LA    R3,ACTTAB                                                        
         LA    R3,HELPENT-ACTTAB(R3) POINT TO HELP ENTRY                        
         B     VALACT4                                                          
         SPACE 1                                                                
VALACT2  CLI   ACTCODE,X'FF'       TEST FOR END-OF-TABLE                        
         BE    VALACTR                                                          
         CLC   ACTCODE,FLD         TEST FOR VALID ACTION                        
         BE    VALACT4             FOUND ONE                                    
         CLC   FLDH+5(1),ACTMINL   TEST ACTUAL LEN VS. MIN                      
         BL    VALACT3             TOO SMALL FOR VARIABLE COMPARE               
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),ACTCODE                                                   
         BE    VALACT4             OK                                           
         SPACE 1                                                                
VALACT3  LA    R3,ACTNTRL(R3)      NEXT TABLE ENTRY                             
         B     VALACT2                                                          
         SPACE 1                                                                
VALACT4  CLI   DDS,YES             DDS, NO SECURITY CHECK NEEDED                
         BE    VALACT5                                                          
         TM    ACTIND,DDSONLY      TEST FOR DDS ONLY ACTION                     
         BNZ   VALACTR             YES, ERROR                                   
         TM    ACTIND,READONLY     IS THIS READ ONLY ACTION                     
         BO    VALACT5             YES, NO SECURITY CHECK NEEDED                
         CLC   ACTCODE,=CL6'UPLOAD'                                             
         BE    VALACT5             BYPASS AUTH CODE CHECK FOR UPLOADS           
         TM    AUTH,X'08'          IS AUTHORIZED TO DO UPDATES                  
         BO    VALACTR             NO, SECURITY ERROR                           
*                                                                               
VALACT5  ST    R3,AACTNTRY         SAVE A(ACTION ENTRY)                         
         B     VALACTX                                                          
         SPACE 1                                                                
VALACTR  MVC   XTRA(6),=C'ACTION'  SET ADDITIONAL ERROR MSG                     
         B     ERROR                                                            
         SPACE 1                                                                
VALACTX  CLI   ACTN,HELP           TEST FOR HELP                                
         BE    OV                  SKIP ALL FIELD VALIDATIONS                   
         B     VALCLI                                                           
         EJECT                                                                  
* VALIDATE CLIENT                                                               
*                                                                               
VALCLI   CLC   ACTCODE,=CL6'UPLOAD'   IF UPLOAD ACTION                          
         BE    OV                  GO DIRECTLY TO OVERLAY                       
*                                                                               
         LA    R2,BUYCLIH                                                       
         MVI   FERN,MISERR         CLIENT IS ALWAYS COMPULSORY                  
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BNE   VALCLI1A                                                         
         TM    ACTCLI,OPTIONAL     TEST IF FIELD IS OPTIONAL                    
         BZ    ERROR               NO                                           
         B     VALEST                                                           
*                                                                               
VALCLI1A MVI   FERN,INVERR                                                      
         GOTO1 VCLPACK,DMCB,FLD,CLIPK                                           
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    ERROR                                                            
* CHECK IF PROFILES HAVE TO BE RE-READ                                          
         L     RE,ATWA                                                          
         A     RE,=A(BASETWA)                                                   
         USING SVAREA,RE                                                        
*                                                                               
         CLC   LSTAGY,AGYALPH                                                   
         BNE   VALCLI1C                                                         
         CLC   LSTCLI,FLD                                                       
         BNE   VALCLI1C                                                         
                                                                                
* MOVE PROFILES INTO THE BLOCK                                                  
         MVC   NBUSER,SVPROF3                                                   
         MVC   NBUSER1,SVPROF1                                                  
         MVC   NBUSER2,SVPROF2                                                  
         MVC   NBINDS9,SVINDS9                                                  
         DROP  RE                                                               
*                                                                               
VALCLI1C MVC   NBSELCL2,CLIPK                                                   
         MVI   NBSELMOD,NBVALCLI                                                
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD      TEST FOR ERROR                               
         BE    VALCLI1                                                          
         MVC   FERN,NBERROR                                                     
         B     ERROR                                                            
         SPACE 1                                                                
VALCLI1  MVC   CLI,FLD             ALPHA CLIENT                                 
         CLC   CLIENT,CLIPK        TEST FOR CHANGE IN CLIENT                    
         BE    *+8                                                              
         MVI   MODE,DISPLAY        FORCE DISPLAY ON CHANGE                      
         MVC   CLIENT,CLIPK                                                     
*                                                                               
         L     R4,NBAIO                                                         
         USING CLTHDRD,R4                                                       
         MVC   BUYCLIN,CNAME       MOVE CLIENT NAME TO SCREEN                   
         OI    BUYCLINH+6,X'80'                                                 
*                                                                               
*  CHECK CLIENT SECURITY                                                        
*                                                                               
         GOTO1 VCHKSEC,DMCB,NBAIO,BUYCLI                                        
*                                                                               
***      OC    TWAACCS(2),TWAACCS  TEST FOR ANY SECURITY LIMITS                 
***      BZ    VALCLI4                                                          
***      CLI   TWAACCS,C'+'        TEST FOR MARKET LOCKOUT                      
***      BE    VALCLI4             NO CHECK                                     
***      MVI   FERN,SCTYERR                                                     
***      CLI   TWAACCS,C'$'        TEST FOR OFFICE LIMIT                        
***      BE    VALCLI2                                                          
***      CLI   TWAACCS,C'*'        TEST FOR OFFICE LIMIT                        
***      BE    VALCLI3                                                          
***      CLC   TWAACCS(2),CLIPK    TEST FOR FILTERED CLIENT                     
***      BE    VALCLI4             OK                                           
***      B     ERROR                                                            
***      SPACE 1                                                                
*                                                                               
***VALCLI2  DS    0H               * TEST OFFICE LIST SECURITY *                
***      XC    DMCB(8),DMCB                                                     
***      MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
***      L     RF,VCALLOV                                                       
***      GOTO1 (RF),DMCB                                                        
***      CLI   4(R1),X'FF'                                                      
***      BNE   *+6                                                              
***      DC    H'0'                                                             
***      XC    DUB,DUB                                                          
***      LA    R1,DUB                                                           
***      USING OFFICED,R1                                                       
***      MVI   OFCSYS,C'S'         SYSTEM ID                                    
***      MVC   OFCAUTH,TWAACCS     ID AUTH VALUE                                
***      MVC   OFCAGY,AGENCY                                                    
***      MVC   OFCOFC,COFFICE                                                   
*                                                                               
***      L     RF,DMCB                                                          
***      GOTO1 (RF),DMCB,DUB,ACOMFACS,0                                         
***      CLI   0(R1),0                                                          
***      BNE   ERROR                                                            
***      B     VALCLI4                                                          
*                                                                               
***VALCLI3  CLC   TWAACCS+1(1),COFFICE TEST FOR FILTERED OFFICE                 
***      BNE   ERROR                                                            
         SPACE 1                                                                
VALCLI4  MVC   CLIPRO,CPROF                                                     
         MVC   CLIEXTRA,CEXTRA                                                  
         MVC   CLIOPT2,COPT2                                                    
         MVC   CLICOST2,CCOST2                                                  
         MVC   CLIFPGRP,CRFPGRP                                                 
         MVC   CLIOPT3,COPT3                                                    
         MVC   CLIOPT4,COPT4                                                    
         MVC   CLICPRD,CPRPRD                                                   
         LA    RE,CLILIST                                                       
         LA    RF,880                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST            PRODUCT LIST                                 
         MVCL  RE,R0               MOVE IT TO BUY VALUES                        
*                                                                               
         LA    RE,CLILIST                                                       
         AH    RE,=H'880'                                                       
         LA    RF,140                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST2           EXTENDED PRODUCT LIST                        
         MVCL  RE,R0               MOVE IT TO BUY VALUES                        
*                                                                               
VALCLI6  L     R2,ATWA                                                          
         A     R2,=A(BASETWA)                                                   
         USING SVAREA,R2                                                        
*                                                                               
         MVC   SVPROF3,NBUSER                                                   
         MVC   SVPROF1,NBUSER1                                                  
         MVC   SVPROF2,NBUSER2                                                  
         MVC   SVINDS9,NBINDS9                                                  
*                                                                               
         MVC   BUYPROF,SVPROF1                                                  
         MVC   BUYPROF2,SVPROF2                                                 
         MVC   NETPROF,SVPROF3                                                  
*                                                                               
         MVC   LSTAGY,AGYALPH                                                   
         MVC   LSTCLI,CLI                                                       
         MVC   LSTOFF,COFFICE                                                   
*                                                                               
VALCLIX  B     VALEST                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
* VALIDATE ESTIMATE                                                             
*                                                                               
VALEST   LA    R2,BUYESTH                                                       
         ST    R2,FADDR                                                         
         MVI   FNDX,0                                                           
         XC    FTERM,FTERM                                                      
         XC    FLAST,FLAST         EDIT FROM START OF FIELD                     
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    VALEST2                                                          
         TM    FLDH+4,X'04'        TEST FOR ALPHA INPUT                         
         BZ    VALEST4             NO                                           
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,3            TEST FOR LENGTH OF 3                         
         BNE   ERROR                                                            
         CLC   =C'ALL',FLD         TEST FOR ALL                                 
         BNE   ERROR                                                            
*                                                                               
VALEST1  MVI   EST,1               SET RANGE OF 1-255 FOR ALL                   
         MVI   EST+1,255                                                        
         MVI   ESTTYP,IALL                                                      
         TM    ACTEST,IALL         TEST IF 'ALL' VALID                          
         BO    VALEST10                                                         
         B     ERROR                                                            
         SPACE 1                                                                
VALEST2  MVI   FERN,MISERR         HANDLE CASE OF NO INPUT                      
         TM    ACTEST,OPTIONAL     TEST IF FIELD OPTIONAL                       
         BZ    ERROR               NO                                           
         TM    ACTEST,DEFAULT      TEST FOR DEFAULT SETTING                     
         BZ    *+14                                                             
         MVC   EST(L'ACTESTD),ACTESTD                                           
         MVI   ESTTYP,DEFAULT                                                   
         B     VALEST10                                                         
         SPACE 1                                                                
VALEST4  TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    VALEST6                                                          
         BAS   RE,ESTNUM                                                        
         STC   R0,EST                                                           
         STC   R0,EST+1                                                         
         MVI   ESTTYP,ISINGLE                                                   
         TM    ACTEST,ISINGLE                                                   
         BO    VALEST10                                                         
         B     ERROR                                                            
         SPACE 1                                                                
VALEST6  XC    FLAST,FLAST         RANGE VALIDATION                             
         MVC   FTERM(2),=C',='     SEARCH FOR COMMA/EQUALS                      
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA STOPPED IT                     
         BNE   VALEST8                                                          
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIRST FIELD                 
         BZ    ERROR                                                            
         BAS   RE,ESTNUM                                                        
         STC   R0,EST                                                           
*                                                                               
VALEST7  XC    FTERM,FTERM         NOW NO TERMINATORS                           
         GOTO1 AFVAL,0                                                          
         TM    FLDH+4,X'08'        TEST IF SECOND FIELD NUMERIC                 
         BZ    ERROR                                                            
         BAS   RE,ESTNUM                                                        
         CLM   R0,1,EST            TEST IF SECOND NUMBER G.T. FIRST             
         BNH   ERROR                                                            
         STC   R0,EST+1                                                         
         MVI   ESTTYP,IRANGE                                                    
         TM    ACTEST,IRANGE                                                    
         BO    VALEST10                                                         
         B     ERROR                                                            
         SPACE 1                                                                
VALEST8  CLI   FSTOP,C'='          FILTER VALIDATION F=(3 BYTES)                
         BNE   ERROR                                                            
         CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         CLI   FLD,C'F'            TEST FOR FILTER PREFIX                       
         BNE   ERROR                                                            
         XC    FTERM,FTERM         GET REST OF FIELD                            
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,3            NO MORE THAN 3 CHARACTERS                    
         BH    ERROR                                                            
         MVI   ESTTYP,IFILTER                                                   
         TM    ACTEST,IFILTER      TEST IF FILTER VALID FOR ACTION              
         BZ    ERROR                                                            
         MVC   EST,FLD             SET FILTER VALUES                            
         B     VALEST10                                                         
         SPACE                                                                  
VALEST10 CLC   ESTIMATE,EST        TEST FOR CHANGED ESTIMATE                    
         BE    *+8                                                              
         MVI   MODE,DISPLAY                                                     
         MVC   ESTIMATE,EST                                                     
         XC    BUYESTD,BUYESTD                                                  
         OI    BUYESTDH+6,X'80'                                                 
         CLI   ESTTYP,ISINGLE      TEST FOR SINGLE NUMBER                       
         BNE   VALESTX                                                          
         SPACE 1                                                                
VALEST12 MVC   NBSELEST,EST                                                     
         MVI   NBSELMOD,NBVALEST                                                
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+14                                                             
         MVC   FERN,NBERROR                                                     
         B     ERROR                                                            
*                                  OUTPUT ESTIMATE VALUES ON SCREEN             
VALEST13 L     R4,NBAIO                                                         
         USING ESTHDRD,R4                                                       
         MVI   FERN,ELOCKERR       TEST FOR LOCKED ESTIMATE                     
         TM    ECNTRL,X'08'                                                     
         BO    ERROR                                                            
         MVC   BUYESTD(20),EDESC   ESTIMATE NAME                                
         MVC   ESTSTART,ESTART     ESTIMATE START (YYMMDD)                      
         MVC   ESTEND,EEND         ESTIMATE END (YYMMDD)                        
         GOTO1 VDATCON,DMCB,ESTART,(5,BUYESTD+22)                               
         MVI   BUYESTD+30,C'-'                                                  
         GOTO1 (RF),(R1),EEND,(5,BUYESTD+31)                                    
         GOTO1 (RF),(R1),ESTART,(2,ESTS)                                        
         GOTO1 (RF),(R1),EEND,(2,ESTE)                                          
         MVC   ESTBOOK,EBOOK       HUT BOOK                                     
         MVC   BUYESTD+43(3),=C'HUT'                                            
         ZIC   R0,ESTBOOK          HUT YEAR FIRST                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUYESTD+47(2),DUB+6(2)                                           
         MVC   BUYESTD+49(3),=C'(9)'  NOW DO PERIOD                             
         MVC   BUYESTD+50(1),ESTBOOK+1                                          
         OI    BUYESTD+50,X'F0'    AND MAKE IT NUMERIC                          
*                                                                               
VALEST14 MVC   ESTDEMSE(60),EDEMLST                                             
         MVC   ESTDEMSE+60(60),EDEMLST1                                         
         MVC   ESTDEMSE+120(30),EDEMLST2                                        
*                                                                               
         CLC   ELEN,=AL2(ESTHDR2Q)                                              
         JL    VALEST15                                                         
         L     RF,NBEXTEND                                                      
         USING NBXTNDD,RF                                                       
         L     RF,NBXCDNL          A(COMSCORE DEMO NAME LIST)                   
         MVC   0(160,RF),ENONTDMS                                               
         DROP  RF                                                               
*                                                                               
VALEST15 LA    RE,DBLOCKA                                                       
         L     RF,=F'256'                                                       
         XCEF                                                                   
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         GOTO1 VDEMOCON,DMCB,(50,ESTDEMSE),('DEMOCON_17',ESTDEMS),     X        
               (C'S',DBLOCK),ESTDEMPL                                           
         DROP  RF                                                               
*                                                                               
         MVC   ESTWLST(20),EWGTLST                                              
         MVC   ESTWLST+20(1),EDEM21WT                                           
         MVC   ESTUSNS,EUSRNMS                                                  
         MVC   ESTWNAM,EWGTNM                                                   
         MVC   ESTFILT,EPROF                                                    
         MVC   ESTSREP,EREP                                                     
         MVC   ESTRATE,ERATE                                                    
         MVC   ESTRATEC,ERATECST                                                
         MVC   ESTCOST2,ECOST2                                                  
         MVC   ESTFLAG1,EFLAG1                                                  
*                                                                               
         LA    R0,MAXNDEMS         MAXIMUM DEMOS                                
         SR    R1,R1               COUNTER                                      
         LA    R2,ESTDEMS                                                       
VALEST20 OC    0(3,R2),0(R2)                                                    
         BZ    *+16                                                             
         LA    R1,1(R1)            INCREMENT DEMO COUNT                         
         LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,VALEST20                                                      
         STC   R1,ESTNDEMS                                                      
         B     VALESTX                                                          
         SPACE 1                                                                
VALESTX  B     VALNET                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO PROCESS ESTIMATE NUMBER (AT ENTRY DUB HAS NUMBER)              
         SPACE 1                                                                
ESTNUM   CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
VALNET   LA    R2,BUYNETH                                                       
         ST    R2,FADDR                                                         
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BNE   VALNET2             FIELD HAS INPUT                              
*                                                                               
         MVI   FERN,MISERR                                                      
         TM    ACTNET,OPTIONAL     TEST IF FIELD IS OPTIONAL                    
         BZ    ERROR               NO                                           
         TM    ACTNET,DEFAULT      TEST FOR SETTING DEFAULT VALUE               
         BZ    *+14                                                             
         MVC   NET,ACTNETD         YES-MOVE IN THE DEFAULT VALUE                
         MVI   NETTYP,DEFAULT                                                   
         B     VALNET6                                                          
         SPACE 1                                                                
VALNET2  MVI   FERN,INVERR                                                      
         CLI   FLDH+5,3            TEST FOR LENGTH OF 3                         
         BNE   VALNET4             NO                                           
         CLC   =C'ALL',FLD         TEST FOR 'ALL'                               
         BNE   VALNET4             NO                                           
         MVI   NETTYP,IALL                                                      
         TM    ACTNET,IALL         TEST IF 'ALL' IS VALID INPUT                 
         BO    VALNET6             YES                                          
         B     ERROR               NO                                           
         SPACE 1                                                                
VALNET4  MVC   NET,FLD             EXTRACT INPUT                                
         TM    FLDH+4,X'04'        MSPACK REQUIRES ALPHA                        
         BZ    ERROR                                                            
         MVI   NETTYP,ISINGLE      SET SWITCH FOR ONE NETWORK                   
         SPACE 1                                                                
VALNET6  CLC   NETWORK,NET         TEST FOR CHANGE                              
         BE    *+8                                                              
         MVI   MODE,DISPLAY                                                     
         MVC   NETWORK,NET                                                      
         CLI   NETTYP,ISINGLE      TEST FOR ONE NETWORK INPUT                   
         BNE   VALNETX             NO-SKIP STATION RECORD READ                  
         SPACE 1                                                                
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NET                                                  
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGYALPH                                                  
         MVC   STAKCLT,CLI                                                      
         GOTO1 AIO,DMCB,STA+READ+FILE,AIOAREA1                                  
*  CHECK IF STATION LOCKED                                                      
         L     R4,AIOAREA1                                                      
*                                                                               
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NBXTNDD,RF                                                       
         OC    NBEXTEND,NBEXTEND                                                
         JZ    *+14                                                             
         L     RF,NBEXTEND                                                      
         MVC   NBXCNN,SCSSTA       COMSCORE NETWORK #                           
         DROP  RF                                                               
*                                                                               
         TM    SFLAG1,SLOCK                                                     
         BZ    VALNET7                                                          
         TM    ACTOP,STALOCK                                                    
         BZ    VALNET7                                                          
         MVI   FERN,STALOKER                                                    
         B     ERROR                                                            
*                                                                               
*  CHECK IF BARTER STATION                                                      
* IF STATION HAS "MIDAS" SETTING CLIENT MUST ALSO BE SET TO "MIDAS"             
*                                                                               
VALNET7  TM    SFLAG1,SMIDAS                                                    
         BZ    VALNET9                                                          
         TM    CLIOPT4,COP4MIDS     IS CLIENT "MIDAS"                           
         BZ    VALNET9                                                          
         OI    HEADFLG1,HMIDFLG     THIS IS A MIDAS REQUEST                     
*                                                                               
VALNET9  L     R4,AIOAREA1                                                      
         MVC   NETFLAG1,SFLAG1                                                  
         MVC   FULL,SMKT           EXTRACT MARKET NUMBER                        
         GOTO1 VMSPACK,DMCB,FULL,STAKCALL,DUB                                   
         MVC   NETMARK,DUB                                                      
         MVC   NBSELNET,NET        SET NETWORK IN NETBLOCK                      
         CLI   STYPE,C'O'                                                       
         BE    *+10                                                             
         MVC   NBSTATYP,STYPE                                                   
         MVC   NBSTSTAT,STYPE                                                   
         MVC   NBPOSTYP,SPTYPE                                                  
         MVC   NBSTPSTT,SPTYPE                                                  
         MVC   NETTRTYP,SPTYPE                                                  
         CLI   STRTYPE,X'40'                                                    
         BNH   *+10                                                             
         MVC   NETTRTYP,STRTYPE                                                 
         MVC   NBSUBMED,SUBMEDIA                                                
         MVC   NBBKTYP,SOVBKTYP                                                 
         CLI   SNTISTA,X'40'                                                    
         BNH   VALNET10                                                         
         MVC   NBNTISTA,SNTISTA    IF EXISTS SET OVERRIDE NTI STATION           
*                                                                               
VALNET10 DS    0H                                                               
         TM    DEMFLAG,D2PRE                                                    
         BZ    VALNET15                                                         
*                                                                               
         OI    NBINDS3,X'40'                                                    
*                                                                               
         MVC   DEMPREC(14),D2PRECNS     INITIALIZE TO NETWORK/SYND.             
         CLI   NBPOSTYP,C'C'                                                    
         BE    VALNET12                                                         
         CLI   NBPOSTYP,C'D'                                                    
         BE    VALNET12                                                         
         CLI   NBPOSTYP,C'O'                                                    
         BNE   VALNETX                                                          
*                                                                               
VALNET12 MVC   DEMPREC(14),D2PRECCO     THEN IT'S CABLE/OTHER                   
         B     VALNETX                                                          
*                                                                               
VALNET15 CLI   NBPOSTYP,C'C'                                                    
         BE    VALNET20                                                         
         CLI   NBPOSTYP,C'D'                                                    
         BE    VALNET20                                                         
         CLI   NBPOSTYP,C'O'                                                    
         BNE   *+22                                                             
VALNET20 MVC   DEMPREC(14),COPREC                                               
         MVI   DEFDPREC,C'C'                                                    
         MVI   NBPREOPT,C'Y'                                                    
         B     VALNETX                                                          
         MVC   DEMPREC(14),NSPREC                                               
         MVI   DEFDPREC,C'N'                                                    
         MVI   NBPREOPT,C'N'                                                    
*                                                                               
VALNETX  DS    0H                                                               
         B     VALPAK                                                           
*                                                                               
***VALNETX  B     VALPAK                                                        
*                                                                               
*--DEMO OVERRIDE PRECISION TABLES                                               
COPREC   DC    C'R',X'82',C'S',X'81',C'P',X'82',C'T',X'42',C'H',X'42'           
         DC    C'U',X'43',C'V',X'40'                                            
NSPREC   DC    C'R',X'81',C'S',X'81',C'P',X'81',C'T',X'43',C'H',X'43'           
         DC    C'U',X'43',C'V',X'40'                                            
D2PRECNS DC    C'R',X'82',C'S',X'81',C'P',X'81',C'T',X'43',C'H',X'43'           
         DC    C'U',X'43',C'V',X'40'                                            
D2PRECCO DC    C'R',X'82',C'S',X'81',C'P',X'81',C'T',X'42',C'H',X'42'           
         DC    C'U',X'43',C'V',X'40'                                            
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PACKAGE                                                              
*                                                                               
VALPAK   LA    R2,BUYPAKH                                                       
         ST    R2,FADDR                                                         
         XC    BUYPAKD,BUYPAKD     CLEAR PACKAGE DATA ON SCREEN                 
         OI    BUYPAKDH+6,X'80'                                                 
         TM    ACTOP,PASSEQ                                                     
         BZ    *+12                                                             
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BE    VALPAK1                                                          
         TM    ACTOP,PUPTRNS                                                    
         BO    *+12                                                             
         TM    ACTPACK,NOEDIT                                                   
         BZ    VALPAK1                                                          
         MVC   BUYPAK,SPACES                                                    
         OI    BUYPAKH+6,X'80'                                                  
         B     VALPAKX                                                          
         SPACE 1                                                                
VALPAK1  GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   VALPAK2                                                          
         MVI   FERN,MISERR                                                      
         TM    ACTOP,PASSEQ                                                     
         BZ    *+12                                                             
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BE    ERROR                                                            
         TM    ACTPACK,OPTIONAL                                                 
         BZ    ERROR               FIELD IS COMPULSORY                          
         TM    ACTPACK,DEFAULT                                                  
         BZ    *+14                NO DEFAULT SETTING                           
         MVC   PACK,ACTPACKD                                                    
         MVI   PACKTYP,DEFAULT     SET TYPE OF INPUT                            
         B     VALPAK6                                                          
         SPACE 1                                                                
VALPAK2  MVI   FERN,INVERR                                                      
         TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    VALPAK4             NO                                           
         LTR   R0,R0               TEST FOR ZERO                                
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,PACK                                                          
         MVI   PACKTYP,ISINGLE                                                  
         TM    ACTOP,PASSEQ                                                     
         BZ    *+12                                                             
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BE    VALPAK6                                                          
         TM    ACTPACK,ISINGLE     TEST IF PACKAGE NUMBER IS VALID              
         BO    VALPAK6             YES                                          
         B     ERROR               NO                                           
         SPACE 1                                                                
VALPAK4  CLI   FLDH+5,3            LOOK FOR 'ALL'                               
         BNE   ERROR                                                            
         CLC   =C'ALL',FLD                                                      
         BNE   ERROR                                                            
         MVI   PACKTYP,IALL                                                     
         TM    ACTPACK,IALL        TEST IF 'ALL' ALLOWED FOR ACTION             
         BO    VALPAK6             YES                                          
         B     ERROR                                                            
         SPACE 1                                                                
VALPAK6  CLC   PACKAGE,PACK        TEST FOR CHANGE IN PACKAGE                   
         BE    *+8                 NO                                           
         MVI   MODE,DISPLAY        YES-FORCE DISPLAY                            
         MVC   PACKAGE,PACK                                                     
         CLI   PACKTYP,ISINGLE     TEST FOR ONE NUMBER                          
         BNE   VALPAKX                                                          
         CLI   ESTTYP,ISINGLE      MUST ALSO HAVE SINGLE                        
         BNE   VALPAKX             ESTIMATE AND NETWORK                         
         CLI   NETTYP,ISINGLE                                                   
         BNE   VALPAKX                                                          
         SPACE 1                                                                
VALPAK7  MVC   NBSELPAK,PACK       NOW READ THE PACKAGE                         
         MVI   NBSELMOD,NBPROCPK                                                
         MVI   NBSELPST,C'B'                                                    
         MVI   NBDATA,C'P'                                                      
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBMODE,NBREQLST     'REQLST' MEANS PACKAGE WAS                   
         BNE   VALPAK8             NOT FOUND                                    
         TM    ACTOP,PASSEQ                                                     
         BZ    *+12                                                             
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BE    VALPAKX                                                          
         MVI   FERN,PAKERR                                                      
         B     ERROR                                                            
         SPACE 1                                                                
VALPAK8  TM    ACTOP,PASSEQ                                                     
         BZ    VALPAK8A                                                         
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BNE   VALPAK8A                                                         
         MVI   FERN,DUPPACK                                                     
         B     ERROR                                                            
VALPAK8A L     R4,NBAIO            DISPLAY PACKAGE DATA ON SCREEN               
         USING NPRECD,R4                                                        
         MVC   PACKCNTL,NPAKCNTL   SAVE PACKAGE CONTROL BYTE                    
         TM    ACTOP,VPHBASE                                                    
         BZ    VALPAK8C                                                         
         TM    NPAKCNTL,X'40'      CHECK IMPR. BASED PACKAGE                    
         BZ    VALPAK8C                                                         
         LA    R3,ACTNTRL(R3)      GET NEXT TABLE ENTRY                         
         ST    R3,AACTNTRY                                                      
*                                                                               
VALPAK8C MVC   BUYPAKD,SPACES                                                   
         MVC   BUYPAKD(L'NBPAKNAM),NBPAKNAM                                     
*                                                                               
         LA    RE,BUYPAKD+22                                                    
         MVC   0(2,RE),NBACTNDP   DAYPART CODE                                  
         LA    RE,1(RE)                                                         
         CLI   0(RE),X'40'                                                      
         BNH   *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         MVC   0(8,RE),NBDPNAM    DAYPART NAME                                  
*                                                                               
         LA    R2,BUYPAKD+34                                                    
         EDIT  (B4,NBPAKCST),(10,(R2)),COMMAS=YES,FLOAT=$,ALIGN=LEFT            
         MVC   BUYPAKD+53(2),=C'48' SET HUT WEEK CALENDAR                       
         TM    NPAKHUTL,X'40'      TEST FOR 52 WEEK CALENDAR                    
         BZ    *+10                                                             
         MVC   BUYPAKD+53(2),=C'52'                                             
*                                                                               
         TM    NPAKSTAT,X'02'      TEST FOR AUDIT                               
         BZ    *+10                                                             
         MVC   BUYPAKD+18(3),=C'AUD'                                            
*                                                                               
         MVC   BUYPAKD+46(4),=C'OPEN'                                           
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN                              
         BZ    *+14                                                             
         MVC   BUYPAKD+46(6),=C'FROZEN'                                         
         B     VALPAK10                                                         
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED                              
         BZ    VALPAK10                                                         
         MVC   BUYPAKD+46(6),=C'LOCKED'                                         
         SPACE 1                                                                
VALPAK10 LA    RE,PACKREC                                                       
         ST    RE,APACKREC         SET POINTER TO PACKAGE                       
         LR    R0,R4                                                            
         LH    R1,NPKRLEN          MOVE IT TO DESIGNATED AREA                   
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',PACKREC),(1,=C'K')             
         CLI   12(R1),0                                                         
         BNE   *+10                                                             
         MVC   APAKFEL,12(R1)                                                   
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'09',PACKREC),0                     
         CLI   12(R1),0                                                         
         BNE   *+10                                                             
         MVC   APAKAEL,12(R1)                                                   
         SPACE                                                                  
         XC    PKGFLAG1,PKGFLAG1               INIT                             
         XC    PKGCOST2,PKGCOST2                                                
         LA    RE,PACKREC+(NPAKEL-NPKEY)       POINT TO FIRST ELEMENT           
         LLC   RF,1(RE)                        BUMP TO SEONDARY INFO            
         AR    RE,RF                           ELEMENT                          
         USING NPK2D,RE                                                         
         MVC   PKGCOST2,NPK2COS2               GET COST2 FACTOR                 
         TM    NPK2FLAG,NPK2FC2I               CHECK FOR ICOS2                  
         JZ    VALPAKX                                                          
         OI    PKGFLAG1,NPK2FC2I               TURN OF INT COST FLAG            
         SPACE 1                                                                
VALPAKX  B     VALPRG                                                           
         DROP  R4,RE                                                            
         EJECT                                                                  
* VALIDATE PROGRAM                                                              
*                                                                               
VALPRG   LA    R2,BUYPRGH                                                       
         ST    R2,FADDR                                                         
         XC    BUYPRGD,BUYPRGD                                                  
         OI    BUYPRGDH+6,X'80'                                                 
         TM    ACTPROG,NOEDIT      TEST FOR SKIPPING EDIT                       
         BZ    VALPRG1             NO                                           
         MVC   BUYPRG,SPACES       YES-SEND BACK CLEARED FIELD                  
         OI    BUYPRGH+6,X'80'                                                  
         B     VALPRGX                                                          
         SPACE                                                                  
VALPRG1  XC    KEY,KEY                                                          
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   VALPRG2             YES                                          
         MVI   FERN,MISERR                                                      
         TM    ACTPROG,OPTIONAL                                                 
         BZ    ERROR               FIELD IS COMPULSORY                          
         TM    ACTPROG,DEFAULT                                                  
         BZ    *+10                                                             
         MVC   PROG,ACTPROGD       SET DEFAULT                                  
         MVI   PROGTYP,DEFAULT                                                  
         B     VALPRG10                                                         
         SPACE 1                                                                
VALPRG2  MVI   FERN,INVERR                                                      
         CLI   FLDH+5,3            TEST FOR 'ALL'                               
         BNE   VALPRG4                                                          
         CLC   =C'ALL',FLD                                                      
         BNE   VALPRG4                                                          
         MVI   PROGTYP,IALL                                                     
         TM    ACTPROG,IALL        IS 'ALL' VALID FOR ACTION                    
         BO    VALPRG10            YES                                          
         B     ERROR               NO                                           
         SPACE 1                                                                
*                                  SEARCH FOR 'F=FILTERS' EXPRESSION            
VALPRG4  GOTO1 VSCANNER,DMCB,FLDH,(1,AIOAREA1),0                                
         CLI   4(R1),0                                                          
         BE    VALPRG6                                                          
         L     R1,AIOAREA1                                                      
         CLI   1(R1),0             TEST FOR SPLIT FIELD                         
         BE    VALPRG6             NO                                           
         CLI   0(R1),1                                                          
         BNE   VALPRG6                                                          
         CLI   12(R1),C'F'         TEST FOR KEYWORD OF 'F'                      
         BNE   VALPRG6                                                          
         MVC   PROG,SPACES                                                      
         MVI   PROGTYP,IFILTER                                                  
         MVC   PROG,22(R1)         FILTER VALUES (POSITIONAL)                   
         TM    ACTPROG,IFILTER     TEST IF FILTERS ARE OK FOR ACTION            
         BZ    ERROR                                                            
         B     VALPRG10                                                         
         SPACE 1                                                                
VALPRG6  TM    ACTPROG,ISINGLE                                                  
         BZ    ERROR                                                            
*                                                                               
         MVC   PROG,FLD            SET INPUT DATA                               
         MVI   PROGTYP,ISINGLE                                                  
         CLI   ESTTYP,ISINGLE      MUST HAVE SINGLE ESTIMATE                    
         BNE   VALPRG10            AND NETWORK FOR PROGRAM VALIDATION           
         CLI   NETTYP,ISINGLE                                                   
         BNE   VALPRG10                                                         
*                                                                               
         XC    KEY,KEY             NOW PRE-VALIDATE PROGRAM BY LOOKING          
         LA    R4,KEY              FOR ANY PROGRAMS APPLICABLE TO               
         USING NPGRECD,R4          ESTIMATE                                     
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,AGYMED                                                    
         MVC   NPGKNET,NETMARK     NETWORK MARKET NUMBER                        
         MVC   NPGKPROG,PROG       INPUT DATA                                   
         MVC   NPGKEND,ESTS        PROGRAMS ENDING ON OR AFTER EST ST           
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(NPGKEND-NPGKEY),KEYSAVE                                      
         BE    VALPRG8             PROGRAM IS VALID                             
         MVI   FERN,PROGERR                                                     
         B     ERROR                                                            
         DROP  R4                                                               
*                                                                               
VALPRG8  DS    0H                                                               
*                                                                               
VALPRG10 DS    0H                                                               
         CLC   PROGRAM,PROG        TEST FOR CHANGE IN PROGRAM                   
         BE    *+8                                                              
         MVI   MODE,DISPLAY                                                     
         MVC   PROGRAM,PROG                                                     
*--READ NAD DEFINITION RECORD THROUGH THE PROGRAM RECORD                        
*--AND FILTER OUT ALL NAD DEMOS THAT ARE NOT DEFINED ON                         
*--THE NAD DEFINITION RECORD                                                    
* SUB-ROUTINE TO READ NADDEF RECORD AND RETRIEVE THE NAD DEMOS                  
*                                                                               
*        CLC   KEY(2),=XL2'0D20'   WAS PROGRAM KEY READ                         
*        BNE   VALPRGX             NO EXIT                                      
*        GOTO1 VGETPROG,DMCB,KEY+11                                             
*        USING NPGRECD,R7                                                       
*        L     R7,APROGREC                                                      
*                                                                               
*        USING NPGEL03,R2                                                       
*        LA    R2,NPGMAINL                                                      
*        ZIC   RE,1(R2)            TEST IF NAD ELEMENT FOUND                    
*        AR    R2,RE               NO                                           
*        CLI   0(R2),X'03'                                                      
*        BNE   VALPRGX                                                          
*        OC    NPGNADDM,NPGNADDM                                                
*        BZ    VALPRGX                                                          
*                                                                               
*        XC    KEY,KEY                                                          
*        MVC   KEY(2),=XL2'0D16'                                                
*        MVC   KEY+2(1),AGYMED                                                  
*        MVC   KEY+3(6),NPGNADDM   NAD DEFINITION CODE                          
*        MVC   KEYSAVE(13),KEY                                                  
*        DROP  R2,R7                                                            
*                                                                               
*        GOTO1 AIO,DMCB,SPT+DIR+HIGH  RE-READ RECORD AND REPLACE                
*        CLC   KEY(13),KEYSAVE                                                  
*        BNE   VALPRGX                                                          
*        GOTO1 (RF),(R1),SPT+FILE+GET,AIOAREA4                                  
*                                                                               
*        USING NNDRECD,R7                                                       
*        L     R7,AIOAREA4                                                      
*        LA    R2,NNDMAINL                                                      
*        XC    HUTBLK,HUTBLK                                                    
*        LA    RE,HUTBLK                                                        
*                                                                               
*ALPRG20 ZIC   RF,1(R2)                                                         
*        AR    R2,RF                                                            
*        CLI   0(R2),0                                                          
*        BE    VALPRG25                                                         
*        CLI   0(R2),X'DD'                                                      
*        BNE   VALPRG20                                                         
*        MVC   0(3,RE),3(R2)                                                    
*        LA    RE,3(RE)                                                         
*        B     VALPRG20                                                         
*                                                                               
*ALPRG25 LA    RE,ESTDEMS                                                       
*        ZIC   RF,ESTNDEMS                                                      
*        LA    R4,FLD                                                           
*        SR    R1,R1                                                            
*                                                                               
*ALPRG30 CLI   0(RE),0             IS DEMO NAD                                  
*        BE    VALPRG50            NO BYPASS                                    
*        LA    R2,HUTBLK                                                        
*        LA    R7,40                                                            
*ALPRG35 OC    0(3,R2),0(R2)                                                    
*        BZ    VALPRG55                                                         
*        CLC   0(1,RE),0(R2)                                                    
*        BNE   VALPRG40                                                         
*        CLC   2(1,RE),2(R2)                                                    
*        BE    VALPRG50                                                         
*ALPRG40 LA    R2,3(R2)                                                         
*        BCT   R7,VALPRG35                                                      
*        B     VALPRG55                                                         
*ALPRG50 MVC   0(3,R4),0(RE)                                                    
*        LA    R4,3(R4)                                                         
*        LA    R1,1(R1)                                                         
*ALPRG55 LA    RE,3(RE)            GET NEXT ESTLIST DEMO                        
*        BCT   RF,VALPRG30                                                      
*        STC   R1,ESTNDEMS                                                      
*                                                                               
*        XC    ESTDEMS,ESTDEMS                                                  
*        ZIC   RF,ESTNDEMS                                                      
*        LTR   RF,RF               ANY DEMOS QUALIFY                            
*        BZ    VALPRGX             NO EXIT                                      
*        LA    R1,ESTDEMS                                                       
*        LA    R2,FLD                                                           
*                                                                               
*ALPRG60 MVC   0(3,R1),0(R2)                                                    
*        LA    R1,3(R1)                                                         
*        LA    R2,3(R2)                                                         
*        BCT   RF,VALPRG60                                                      
*                                                                               
         SPACE 1                                                                
VALPRGX  XC    HUTBLK,HUTBLK                                                    
         B     VLOC                                                             
*        DROP  R7                                                               
         EJECT                                                                  
* COMMON ROUTINE TO LOCK RECS BY TYPE, OR CHECK TO SEE IF LOCKED *              
         SPACE                                                                  
VLOC     TM    ACTOP,LOKCHEK                                                    
         BZ    VLOCEX                                                           
*                                                                               
         MVI   BILLOCK,C'N'                                                     
*                                                                               
         L     R4,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R4)                                        
         GOTO1 (RF),DMCB,0                                                      
*                                                                               
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         L     R4,ACOMFACS                                                      
         LA    R3,WORK                                                          
         USING LKKEYD,R3                                                        
         XC    WORK,WORK                                                        
*                                                                               
         MVC   LOCKSE,FASYS                                                     
         MVC   SECAGYA,FATAGYSC     SAVE SECURITY AGENCY                        
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVPASSWD,FAPASSWD   SAVE PERSONAL ID                             
         DROP  RE                                                               
         SPACE                                                                  
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=CL2'UN'                                                 
         MVC   LOCKKEY(3),CLI                                                   
         OI    LOCKKEY+2,X'40'                                                  
         XC    LOCKKEY+3(7),LOCKKEY+3                                           
         SPACE                                                                  
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         SPACE                                                                  
         PRINT GEN                                                              
         GOTO1 (R2),(R1),(C'W',WORK),(R4)                                       
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   VLOCERR                                                          
*                                                                               
*  CHECK STATION LEVEL LOCKS                                                    
*                                                                               
         MVC   LOCKKEY+3(4),NET                                                 
         GOTO1 (R2),(R1),(C'W',WORK),(R4)                                       
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   VLOCERR                                                          
*                                                                               
*  CHECK SOON BILLING LOCKS                                                     
*                                                                               
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=CL2'NB'                                                 
         XC    LOCKKEY(10),LOCKKEY                                              
* CHECK CLIENT LEVEL LOCK                                                       
         MVC   LOCKKEY(3),CLI                                                   
         OI    LOCKKEY+2,X'40'                                                  
         GOTO1 (R2),(R1),(C'T',WORK),(R4)                                       
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    *+12                                                             
         MVI   BILLOCK,C'Y'                                                     
         B     VLOCEX                                                           
* CHECK CLIENT/NETWORK LEVEL LOCK                                               
*****    MVC   LOCKKEY+6(4),NET                                                 
*****    GOTO1 (R2),(R1),(C'W',WORK),(R4)                                       
*****    CLI   DMCB+4,0            ANY ERRORS                                   
*****    BE    *+12                                                             
*****    MVI   BILLOCK,C'Y'                                                     
*****    B     VLOCEX                                                           
* CHECK CLIENT/ESTIMATE/NETWORK LEVEL LOCK                                      
*****    PRINT GEN                                                              
*****    EDIT  (1,EST),(3,8(R3)),FILL=0,WRK=WORK+30                             
*****    PRINT NOGEN                                                            
*****    GOTO1 (R2),(R1),(C'W',WORK),(R4)                                       
*****    CLI   DMCB+4,0            ANY ERRORS                                   
*****    BE    *+12                                                             
*****    MVI   BILLOCK,C'Y'                                                     
*****    B     VLOCEX                                                           
* CHECK CLIENT/ESTIMATE LEVEL LOCK                                              
*****    XC    LOCKKEY+6(4),LOCKKEY+6                                           
*****    GOTO1 (R2),(R1),(C'W',WORK),(R4)                                       
*****    CLI   DMCB+4,0            ANY ERRORS                                   
*****    BE    *+8                                                              
*****    MVI   BILLOCK,C'Y'                                                     
VLOCEX   B     OV                                                               
         DROP  R3                                                               
*--LOCK  ERROR                                                                  
VLOCERR  MVI   FERN,CLLOKERR                                                    
         B     ERROR                                                            
         EJECT                                                                  
* INTERFACE TO OVERLAY                                                          
*                                                                               
OV       L     R3,AACTNTRY         ENTRY LINE                                   
         USING ACTTABD,R3                                                       
         MVC   ACTION,ACTN         ACTION NUMBER                                
         SPACE 1                                                                
OV2      CLC   ACTSCR,SCREEN       TEST IF SCREEN ALREADY LOADED                
         BE    OV4                 YES                                          
         OI    MODE,FIRST          SET FIRST TIME FOR SCREEN                    
         CLI   ACTSCR,0            TEST FOR SELF-GENERATING SCREEN              
         BE    OV3                                                              
         MVC   DMCB+4(3),=X'D90311'                                             
         MVC   DMCB+7(1),ACTSCR    SCREEN NUMBER                                
         GOTO1 VCALLOV,DMCB,BUYLAST                                             
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
OV3      MVC   SCREEN,ACTSCR                                                    
         TM    ACTIND,FSTCURS      TEST FOR SETTING CURSOR/EXIT                 
         BZ    OV4                 NO                                           
         SR    R0,R0                                                            
         LA    R2,BUYACTH          POINT TO ACTION FIELD                        
         IC    R0,0(R2)            AND FIND THE FIRST UNPROTECTED FIELD         
         AR    R2,R0               THAT FOLLOWS IT                              
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         ST    R2,FADDR            CURSOR ADDRESS                               
         MVC   BUYMSG(13),=C'ENTER DETAILS'                                     
         B     OVX                                                              
         SPACE 1                                                                
OV4      CLI   ACTN,HELP           TEST FOR HELP                                
         BE    ACTHELP             DIVERT TO HELP ROUTINE                       
         MVC   OVLAYNUM,ACTOV      SAVE OVERLAY NUMBER                          
*                                                                               
         PRINT GEN                                                              
         GOTO1 VCALLOV,DMCB,(ACTOV,0),ATWA                                      
         PRINT NOGEN                                                            
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             OVERLAY ADDRESS                              
         GOTO1 (RF),DMCB,BUYWRKD                                                
         SPACE 1                                                                
OVX      NI    MODE,X'FF'-FIRST    MAKE SURE FIRST IS OFF                       
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO PRODUCE ACTION HELP DISPLAY                                        
*                                                                               
ACTHELP  GOTO1 VCLEARF,DMCB,(1,HELHED1H),HELLAST                                
         L     R3,=A(ACTTAB)                                                    
         A     R3,BRELO                                                         
*        LA    R3,ACTTAB                                                        
         MVC   HELHED1(6),=C'ACTION'                                            
         MVC   HELHED1+16(3),=C'USE'                                            
         MVC   HELHED1+40(6),=C'ACTION'                                         
         MVC   HELHED1+56(3),=C'USE'                                            
         MVI   BYTE,NO             SET RIGHT HAND SIDE SWITCH                   
         LA    R2,HELLIN1                                                       
         SPACE 1                                                                
ACTHELP1 LA    R0,LINES                                                         
         SPACE 1                                                                
ACTHELP2 CLI   ACTCODE,X'FF'       TEST FOR E-O-T                               
         BE    ACTHELPX                                                         
         CLI   ACTN,HELP                                                        
         BE    *+20                SKIP ITS ENTRY                               
         TM    ACTIND,DDSONLY      TEST FOR DDS ONLY ACTION                     
         BZ    ACTHELP3            NO                                           
         CLI   DDS,YES             YES-NOW CHECK FOR DDS TERMINAL               
         BE    ACTHELP3            YES-SHOW IT IN LIST                          
         LA    R3,ACTNTRL(R3)                                                   
         B     ACTHELP2                                                         
         SPACE 1                                                                
ACTHELP3 LA    R1,L'ACTCODE        FIND LENGTH OF ACTION CODE                   
         LA    RE,ACTCODE+L'ACTCODE-1                                           
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         MVC   WORK,SPACES         PRE-CLEAR SCREEN OUTPUT                      
         BCTR  R1,0                                                             
         EX    R1,HELPMOVE         MOVE OUT ENTIRE CODE                         
         LA    R1,1(R1)            RESTORE ACTUAL LENGTH                        
         CLM   R1,1,ACTMINL        TEST ACTUAL LEN SAME AS MINIMUM              
         BE    ACTHELP5            YES                                          
         LR    R4,R1               SAVE ACTUAL LENGTH                           
         IC    R1,ACTMINL                                                       
         BCTR  R1,0                                                             
         EX    R1,HELPMOVE         EXTRACT THE MINIMUM CHARACTERS               
         LA    R1,1(R1)            RESTORE LENGTH                               
         SPACE 1                                                                
ACTHELP4 SR    R4,R1               FIND LENGTH OF OPTIONAL CHARACTERS           
         LA    RE,WORK(R1)                                                      
         MVI   0(RE),C'('          ENCLOSE OPTIONAL CHARS IN PARENS             
         LA    RF,ACTCODE(R1)      POINT PAST MIN CHARS IN ACTCODE              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),0(RF)       EXTRACT OPTIONAL CHARACTERS                  
         LA    RE,2(R4,RE)         POINT TO END OF STRING                       
         MVI   0(RE),C')'                                                       
         LA    R1,3(R4,R1)         TOTAL LENGTH SO FAR                          
         SPACE 1                                                                
ACTHELP5 LA    R4,WORK(R1)         POINT PAST ACTION CODE AND                   
         CLC   ACTINDES,SPACES     TEST FOR ACTION INPUT DESCRIPTN              
         BE    *+14                NONE                                         
         MVI   0(R4),COMMA         ATTACH ACTION INPUT DESCRIPTION              
         MVC   1(L'ACTINDES,R4),ACTINDES                                        
         MVC   0(14,R2),WORK       WHOLE EXPRESSION TO SCREEN                   
         MVC   16(L'ACTDESC,R2),ACTDESC                                         
         SPACE 1                                                                
ACTHELP6 LA    R3,ACTNTRL(R3)      NEXT TABLE ENTRY                             
         LA    R2,LINELEN(R2)      NEXT SCREEN LINE                             
         BCT   R0,ACTHELP2                                                      
*                                                                               
         CLI   BYTE,YES            TEST IF RIGHT SIDE DONE                      
         BE    ACTHELPX            YES                                          
         MVI   BYTE,YES            NO-SET SWITCH AND DO IT                      
         LA    R2,HELLIN1+40       POSITION TO START ON 1ST LINE                
         B     ACTHELP1                                                         
         SPACE 1                                                                
ACTHELPX MVI   FERN,HELPMSG                                                     
         B     ERROR                                                            
         SPACE 2                                                                
HELPMOVE MVC   WORK(0),ACTCODE                                                  
         DROP  R3                                                               
         EJECT                                                                  
         EJECT                                                                  
* LINKAGE TO COMMON SUB-ROUTINES                                                
*                                                                               
COMMIN   NTR1  BASE=BASE1                                                       
         L     RA,BASE2                                                         
         L     R7,BASE3                                                         
         SRL   RF,24               SHIFT BRANCH INDEX TO L.O.B.                 
         B     COMTAB(RF)                                                       
         SPACE 1                                                                
COMTAB   B     GETFLD                                                           
         B     ERROR                                                            
         B     EXIT                                                             
         B     CLEARF                                                           
         B     GETPROG                                                          
         B     DISPROG                                                          
         B     BLDFLD                                                           
         B     CENFLD                                                           
         B     GETNADS                                                          
         B     CKREASN                                                          
         B     BLDRQST                                                          
         B     BLDTVQ                                                           
         B     HNDLDPT                                                          
         B     CHKAIR                                                           
         B     CHKSEC                                                           
         B     CHGBILL                                                          
         B     GETFLT                                                           
         B     DEDEND                                                           
COMMONS  EQU   (*-COMTAB)/4                                                     
         EJECT                                                                  
* GETFLD - EXTRACT DATA FROM SCREEN FIELD                                       
*                                                                               
* ON ENTRY R2 POINTS TO FIELD HEADER                                            
* ON EXIT                                                                       
*        FADDR = A(FIELD HEADER)                                                
*        FLDH  CONTAINS FIELD HEADER                                            
*        FLD   CONTAINS EXTRACTED FIELD DATA SPACE FILLED                       
*        R0    CONTAINS BINARY VALUE OF DATA IF FIELD IS NUMERIC                
*                                                                               
GETFLD   BRAS  RE,GETFLDR                                                       
         XIT1  REGS=(R0)                                                        
*&&DO                                                                           
GETFLD   ST    R2,FADDR                                                         
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         EX    R1,FLDMOVE                                                       
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GETFLDX                                                          
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
         BZ    GETFLDX                                                          
         CLI   FLDH+5,15           NO MORE THAN 15 DIGITS                       
         BNH   *+12                                                             
         NI    FLDH+4,X'FF'-X'08'  TURN OFF NUMERIC BIT                         
         B     GETFLDX                                                          
         BCTR  R1,0                                                             
         EX    R1,FLDPACK                                                       
         CVB   R0,DUB                                                           
         SPACE 1                                                                
GETFLDX  XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
FLDMOVE  MVC   FLD(0),8(R2)                                                     
FLDPACK  PACK  DUB,FLD(0)                                                       
         EJECT                                                                  
*&&                                                                             
* ERROR - SET ERROR MESSAGE AND EXIT                                            
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER OF FIELD IN ERROR)                              
*        FERN  = SYSTEM ERROR NUMBER OR X'FF' FOR USER MESSAGE                  
*        FNDX  = OPTIONALLY SET FIELD INDEX FOR MULTI-FIELD TWA FIELD           
*        XTRA  = OPTIONALLY CONTAINS EXTRA MESSAGE CONCATENATED TO              
*                SYSTEM ERROR MESSAGE                                           
*                                                                               
ERROR    CLC   BUYACT(2),=CL2'ST'  STEWARD REQUEST                              
         BE    ERROR5                                                           
         CLI   FERN,USERERR        TEST FOR USER MESSAGE                        
         BE    EXIT                                                             
         GOTO1 VGETMSG,DMCB+12,(FERN,BUYMSG),(X'FF',DMCB),(7,0)                 
         LA    R2,BUYMSG+L'BUYMSG-1 R2 POINTS TO END OF MSG FLD                 
         LA    R3,L'BUYMSG         CALCULATE MESSAGE LENGTH                     
         CLI   0(R2),C' '          TEST FOR BLANK                               
         BNE   *+10                                                             
         BCTR  R2,0                BACK UP POINTER                              
         BCT   R3,*-10                                                          
         LA    R2,1(R2)            POINT TO BLANK AFTER LAST CHAR               
*                                                                               
ERROR2   CLI   FNDX,0              TEST FOR INDEX NUMBER                        
         BE    ERROR4                                                           
         LA    R0,L'BUYMSG                                                      
         LA    R3,8(R3)            ADD ON LENGTH OF INDEX MSG +1                
         CR    R3,R0               TEST FOR FIT IN FIELD                        
         BH    EXIT                NO - EXIT                                    
         LA    R2,BUYMSG-7(R3)                                                  
         MVC   0(7,R2),=C'- FLD#N'                                              
         EDIT  (B1,FNDX),(1,6(R2))                                              
         LA    R2,7(R2)            POINT TO BLANK AFTER INDEX MSG               
*                                                                               
ERROR4   CLC   XTRA,SPACES         TEST FOR ANY EXTRA MESSAGE                   
         BE    EXIT                                                             
         LA    RE,XTRA+L'XTRA-1                                                 
         LA    R1,L'XTRA           CALCULATE LENGTH OF EXTRA MESSAGE            
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         LA    R0,L'BUYMSG                                                      
         LA    R3,1(R1,R3)         COMPUTE TOTAL MESSAGE LENGTH                 
         CR    R3,R0               TEST FOR FIT                                 
         BH    EXIT                                                             
         BCTR  R1,0                LESS ONE FOR EXECUTE                         
         EX    R1,*+8              MOVE XTRA TO MESSAGE FIELD                   
         B     EXIT                                                             
         MVC   1(0,R2),XTRA        EXECUTED                                     
         B     EXIT                                                             
*                                                                               
ERROR5   LA    R3,HUTBLK                                                        
         USING ERRDATA,R3                                                       
         MVC   ERMSSEQN,STEWSEQ     RECORD SEQUENCE NUMBER                      
*                                                                               
         MVC   ERNUMBR,FERN                                                     
*  CHECK FOR USER ERROR                                                         
         CLI   FERN,USERERR        TEST FOR USER MESSAGE                        
         BNE   *+18                                                             
         MVI   ERNUMBR,2           INVALID INPUT ERROR                          
         MVC   ERMXTRA,BUYMSG                                                   
         B     ERROR9                                                           
*  MOVE IN EXTRA FIELD                                                          
         MVC   ERMXFLD(6),=CL6'FIELD='                                          
*  GET THE FIELD NAME                                                           
         OC    FADDR,FADDR                                                      
         BZ    ERROR9                                                           
         L     RF,FADDR                                                         
         LA    R2,BUYSRVH                                                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                POINT TO CLIENT NAME FIELD                  
*                                                                               
ERROR7   MVC   ERMXTRA,SPACES                                                   
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,MVSTEXTR                                                      
*  BUMP TO NEXT FIELD                                                           
ERROR8   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,RF                                                            
         BE    ERROR9                                                           
         TM    1(R2),X'20'                                                      
         BZ    ERROR8                                                           
         B     ERROR7                                                           
*                                                                               
*  EXTRA FIELDS FOR FRONTRUNNER ERRORS                                          
ERROR9   CLC   BUYACT(4),=CL4'STFR'  STEWARD/FRONTRUNNER REQUEST                
         BNE   ERROR10                                                          
         MVC   ERCLINT,CLI                                                      
         MVC   ERESTMT,EST                                                      
         MVC   ERNTWK,NET                                                       
         MVC   ERPROG,PROG                                                      
         MVC   ERPKGE,PACK                                                      
         MVC   ERDATE,STEWDAT                                                   
         MVC   ERLNGTH,STEWLEN                                                  
ERROR10  GOTO1 VGLOBBER,DMCB,=C'PUTD',HUTBLK,126,GLVBUY4                        
*                                                                               
         L     RD,AWORK            RESTORE ROOT'S RD                            
         B     EXXMOD                                                           
MVSTEXTR MVC   ERMXTRA(0),8(R2)                                                 
         DROP  R3                                                               
         EJECT                                                                  
* EXIT - SET CURSOR AND EXIT DIRECTLY TO USER                                   
*                                                                               
EXIT     L     R2,FADDR            SET CURSOR                                   
         OI    6(R2),X'C0'         CURSOR AND TRANSMIT BITS                     
         L     RD,AWORK            RESTORE ROOT'S RD                            
         B     EXXMOD                                                           
         EJECT                                                                  
* CLEARF - CLEAR AND FOUT FIELDS                                                
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
CLEARF   LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLEARF2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLEARF4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLEARF2             NO-CONTINUE                                  
         B     EXXMOD              YES-ALL DONE                                 
         SPACE 2                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLEARF4                                                        
         EJECT                                                                  
* GETPROG - GET APPLICABLE PROGRAM RECORD IF NECESSARY                          
*                                                                               
* AT ENTRY P1 CONTAINS A(COMPRESSED DATE FOR READ)                              
*                                                                               
GETPROG  DS    0H                                                               
         L     RE,0(R1)            POINT TO DATE                                
         ICM   R0,3,0(RE)          DATE                                         
         STH   R0,HALF                                                          
         XC    KEY,KEY                                                          
         MVI   FERN,0              CLEAR ERROR BYTE ON ENTRANCE                 
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,AGYMED                                                    
         MVC   NPGKNET,NETMARK                                                  
         MVC   NPGKPROG,PROG                                                    
         MVC   NPGKEND,HALF                                                     
         L     R2,APROGREC                                                      
         CLC   NPGKEY,0(R2)        TEST IF KEY CHANGED                          
         BE    GETPROGX            NO-ALREADY HAVE RIGHT RECORD                 
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
*                                                                               
*        CLC   KEY(L'NPGKEY),0(R2) TEST IF SAME RECORD APPLIES                  
*        BE    GETPROGX            YES                                          
GETPRG1  CLC   KEY(NPGKEND-NPGKEY),KEYSAVE TEST IF SAME PROGRAM                 
         BE    GETPRG3                                                          
*                                                                               
         MVI   FERN,PROGERR                                                     
         B     GETPROGR                                                         
*                                                                               
GETPRG2  GOTO1 AIO,DMCB,SPT+DIR+SEQ                                             
         B     GETPRG1                                                          
         SPACE                                                                  
********GETPRG3  GOTO1 AIO,DMCB,SPT+FILE+GET,(R2)                               
GETPRG3  L     R4,=AL4(IOBUFFER-BUYWRKD)                                        
         LA    R4,BUYWRKD(R4)      SET ADCON FOR WORK BUFFER                    
         GOTO1 AIO,DMCB,SPT+FILE+GET,(R4)                                       
         GOTO1 VHELLO,DMCB,(C'D',SPTFILE),(X'A0',(R4)),0                        
         GOTO1 VHELLO,DMCB,(C'D',SPTFILE),(X'A1',(R4)),0                        
         GOTO1 VHELLO,DMCB,(C'D',SPTFILE),(X'A9',(R4)),0                        
         USING NPGRECD,R4                                                       
         SR    R1,R1                                                            
         ICM   R1,3,NPGRLEN                                                     
         LR    RF,R1                                                            
         L     RE,APROGREC                                                      
         LR    R0,R4                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   ELCODE,NPG3ELQ      BOOK ELEMENT SEARCH                          
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BNE   GETPRG3A                                                         
         ICM   RE,15,12(R1)                                                     
         USING NPGEL03,RE                                                       
         CLI   NPGLEN3,NPG3LNQ2                                                 
         JL    GETPRG3A                                                         
*                                                                               
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NBXTNDD,RF                                                       
         OC    NBEXTEND,NBEXTEND                                                
         JZ    *+14                                                             
         L     RF,NBEXTEND                                                      
         MVC   NBXCSN,NPGCSN       COMSCORE SERIES #                            
         DROP  RF                                                               
*                                                                               
GETPRG3A MVI   ELCODE,X'93'        BOOK ELEMENT SEARCH                          
*****    GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(ELCODE,(R2)),0                       
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BNE   GETPRG4                                                          
         ICM   RE,15,12(R1)                                                     
         USING NPG2ELEM,RE                                                      
         OC    NPG2STD,NPG2STD                                                  
         BZ    GETPRG4                                                          
         CLC   HALF,NPG2STD      MAKE SURE INPUT DATE > PRG START DATE          
         BL    GETPRG2                                                          
         DROP  RE                                                               
*                                                                               
GETPRG4  MVI   ELCODE,X'5D'        BOOK ELEMENT SEARCH                          
******   GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(ELCODE,(R2)),0                       
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ABOOKEL,12(R1)      SAVE ELEMENT ADCON                           
*                                                                               
GETPRG5  DS    0H                                                               
         MVI   ELCODE,X'92'        PROGRAM ELEMENT SEARCH                       
******   GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(ELCODE,(R2)),0                       
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ONE                                
         MVC   APROGEL,12(R1)      SAVE ELEMENT ADCON                           
         B     GETPROGX                                                         
         SPACE                                                                  
GETPROGR L     R3,AACTNTRY                                                      
         USING ACTTABD,R3                                                       
         TM    ACTIND,SETPROGR     TEST FOR HANDLING PROGRAM ERROR              
         BZ    GETPROGX            NO                                           
         LA    R2,BUYPRGH          YES-SET CURSOR POSITION AND EXIT             
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE                                                                  
GETPROGX B     EXXMOD                                                           
         DROP  R3,R4                                                            
*                                                                               
*  GET PROGRAM RECORD ELEMENTS                                                  
GTPRGEL  ST    RE,FULL                                                          
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(ELCODE,(R2)),0                       
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*  THIS ROUTINE HAS BEEN REPLACED BY THE UPOVE GETPROG                          
*                                                                               
*                                                                               
* GETPROG - GET APPLICABLE PROGRAM RECORD IF NECESSARY                          
*                                                                               
* AT ENTRY P1 CONTAINS A(COMPRESSED DATE FOR READ)                              
*                                                                               
****GETPROG  DS    0H                                                           
****         L     RE,0(R1)            POINT TO DATE                            
****         ICM   R0,3,0(RE)          DATE                                     
****         STH   R0,HALF                                                      
****         XC    KEY,KEY                                                      
****         MVI   FERN,0              CLEAR ERROR BYTE ON ENTRANCE             
****         LA    R4,KEY                                                       
****         USING NPGRECD,R4                                                   
****         MVC   NPGKTYP,=X'0D20'                                             
****         MVC   NPGKAM,AGYMED                                                
****         MVC   NPGKNET,NETMARK                                              
****         MVC   NPGKPROG,PROG                                                
****         MVC   NPGKEND,HALF                                                 
****         L     R2,APROGREC                                                  
****         CLC   NPGKEY,0(R2)        TEST IF KEY CHANGED                      
****         BE    GETPROGX            NO-ALREADY HAVE RIGHT RECORD             
****         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                        
*                                                                               
*        CLC   KEY(L'NPGKEY),0(R2) TEST IF SAME RECORD APPLIES                  
*        BE    GETPROGX            YES                                          
****GETPRG1  CLC   KEY(NPGKEND-NPGKEY),KEYSAVE TEST IF SAME PROGRAM             
****         BE    GETPRG3                                                      
*                                                                               
****         MVI   FERN,PROGERR                                                 
****         B     GETPROGR                                                     
*                                                                               
****GETPRG2  GOTO1 AIO,DMCB,SPT+DIR+SEQ                                         
****         B     GETPRG1                                                      
****         SPACE                                                              
****GETPRG3  GOTO1 AIO,DMCB,SPT+FILE+GET,(R2)                                   
****         SPACE                                                              
****         MVI   ELCODE,X'93'        BOOK ELEMENT SEARCH                      
****         BAS   RE,GTPRGEL                                                   
****         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH               
****         BNE   GETPRG4                                                      
****         ICM   RE,15,12(R1)                                                 
****         USING NPG2ELEM,RE                                                  
****         OC    NPG2STD,NPG2STD                                              
****         BZ    GETPRG4                                                      
****         CLC   HALF,NPG2STD      MAKE SURE INP DATE > PRG STRT DTE          
****         BL    GETPRG2                                                      
****         DROP  RE                                                           
*                                                                               
****GETPRG4  MVI   ELCODE,X'5D'        BOOK ELEMENT SEARCH                      
****         BAS   RE,GTPRGEL                                                   
****         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH               
****         BE    *+6                                                          
****         DC    H'0'                                                         
****         MVC   ABOOKEL,12(R1)      SAVE ELEMENT ADCON                       
*                                                                               
****GETPRG5  DS    0H                                                           
****         MVI   ELCODE,X'92'        PROGRAM ELEMENT SEARCH                   
****         BAS   RE,GTPRGEL                                                   
****         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH               
****         BE    *+6                                                          
****         DC    H'0'                MUST HAVE ONE                            
****         MVC   APROGEL,12(R1)      SAVE ELEMENT ADCON                       
****         B     GETPROGX                                                     
****         SPACE                                                              
****GETPROGR L     R3,AACTNTRY                                                  
****         USING ACTTABD,R3                                                   
****         TM    ACTIND,SETPROGR     TEST FOR HANDLING PROGRAM ERROR          
****         BZ    GETPROGX            NO                                       
****         LA    R2,BUYPRGH          YES-SET CURSOR POSITION AND EXIT         
****         ST    R2,FADDR                                                     
****         B     ERROR                                                        
****         SPACE                                                              
****GETPROGX B     EXXMOD                                                       
****         DROP  R3,R4                                                        
*                                                                               
*  GET PROGRAM RECORD ELEMENTS                                                  
****GTPRGEL  NTR1                                                               
****         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(ELCODE,(R2)),0                   
****         B     EXXMOD                                                       
         EJECT                                                                  
* DISPROG - DISPLAY PROGRAM RECORD DATA ON BASE SCREEN                          
*                                                                               
* ON ENTRY - APROGEL POINTS TO PROGRAM ELEMENT                                  
*                                                                               
DISPROG  OC    BUYPRGD,BUYPRGD     TEST IF FIELD HAS BEEN DONE                  
         BNZ   DISPROGX            YES                                          
         SPACE 1                                                                
DISPROG2 L     R4,APROGEL          ELEMENT ADDRESS                              
         USING NPGELEM,R4                                                       
         MVC   BUYPRGD,SPACES      PRE-CLEAR FIELD TO SPACES                    
         MVC   BUYPRGD(L'NPGNAME),NPGNAME                                       
         MVC   DUB,SPACES          GET DAY(S)                                   
         GOTO1 VUNDAY,DMCB,NPGDAY,DUB                                           
         MVC   BUYPRGD+22(8),DUB                                                
         MVC   WORK(11),SPACES     GET START/END TIME                           
         GOTO1 VUNTIME,DMCB,NPGTIME,WORK                                        
         MVC   BUYPRGD+31(11),WORK                                              
*                                                                               
         L     R2,APROGREC                                                      
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'5D',(R2)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     RE,12(R1)                                                        
*                                                                               
         OC    NPGSHARE,NPGSHARE       ANY RATING OR SHARE?                     
         BZ    DISPROGX                                                         
*                                                                               
         LA    R2,BUYPRGD+43       POINT TO START OF R1/S1 OUTPUT               
         TM    NPGSTAT,X'80'           RATING?                                  
         BO    DP210                                                            
*                                                                               
         TM    SVAGYFL2,SVAGYFL2_2DP   2 DEC PRECISION?                         
         BO    DP2200                                                           
         TM    SVAGYFL2,SVAGYFL2_BDP   USER DEFINED?                            
         BZ    DP2200                                                           
*                                                                               
         CLC   5(2,RE),=X'5901'        NEW STYLE SHARE?                         
         BE    DP2200                                                           
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
         B     DP2200                                                           
*                                                                               
DP210    DS    0H                                                               
         TM    SVAGYFL2,SVAGYFL2_2DP   2 DEC PRECISION?                         
         BNZ   DP220                                                            
         TM    SVAGYFL2,SVAGYFL2_BDP   USER DEFINED PRECISION?                  
         BNZ   DP250                                                            
         MVI   0(R2),C'R'              MUST BE 1 DECIMAL PRECISION              
         LA    R2,1(R2)                                                         
         B     DP2200                                                           
*                                                                               
DP220    DS    0H                                                               
         TM    SVAGYFL2,SVAGYFL2_2DP   2 DEC PRECISION?                         
         BZ    DP250                                                            
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
         B     DP2220                                                           
*                                                                               
DP250    DS    0H                                                               
         TM    SVAGYFL2,SVAGYFL2_BDP   2 DEC PRECISION?                         
         BNZ   *+6                                                              
         DC    H'00'                   SOMETHING'S WRONG                        
*                                                                               
         CLC   5(2,RE),=X'5901'        1 DEC PRECSION?                          
         BE    DP260                                                            
         MVC   0(2,R2),=C'1R'                                                   
         LA    R2,2(R2)                                                         
         B     DP2200                                                           
*                                                                               
DP260    DS    0H                                                               
         CLC   5(2,RE),=X'5901'        2 DEC PRECISION?                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   0(2,R2),=C'2R'                                                   
         LA    R2,2(R2)                                                         
         B     DP2220                                                           
*                                                                               
DP2200   DS    0H                      DISPLAY 1 DECIMAL PRECISION              
         EDIT  (B2,NPGSHARE),(5,0(R2)),1,ALIGN=LEFT                             
         B     DP2500                                                           
*                                                                               
DP2220   DS    0H                      DISPLAY 2 DECIMAL PRECISION              
         EDIT  (B2,NPGSHARE),(5,0(R2)),2,ALIGN=LEFT                             
         B     DP2500                                                           
*                                                                               
DP2500   DS    0H                                                               
*                                                                               
DISPROGX B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* BLDFLD - DYNAMICALLY BUILD TWA FIELDS                                         
*                                                                               
* ON ENTRY -                                                                    
*                                                                               
* BLDROW HAS START ROW, BLDCOL HAS START COLUMN, BLDTWAX POINTS TO              
* END OF AVAILABLE TWA, BLDTWAS CAN BE OPTIONALLY SET TO START POINT            
* FOR BUILDING FIELDS IN TWA - THE DEFAULT IS BUYLAST                           
*                                                                               
* EACH BLDLIST ENTRY IS 8 BYTES.                                                
*  BYTE 0 = NUMBER OF LEADING SPACES                                            
*       1 = DATA LENGTH                                                         
*       2 = ATTRIBUTE BYTE (PROT/UNP)                                           
*       3 = NUMBER OF TRAILING SPACES                                           
*       4 = SPARE                                                               
*     5-7 = DATA ADDRESS (OR 0)                                                 
*                                                                               
*  ALL FIELDS MUST FIT THIS LINE OR USE NEXT LINE.                              
* XL4'00' IS E-O-L                                                              
*                                                                               
* ON EXIT CC IS = IF FIELD(S) ADDED TO TWA, NEQ IF UNSUCCESSFUL                 
* AND R2 POINTS TO END OF SCREEN                                                
*                                                                               
BLDFLD   OC    BLDROW,BLDROW       TEST FOR START AT DEFAULT POSITION           
         BNZ   BLDF1                                                            
         LA    R2,BUYLAST          POSITION TO END OF BASE SCREEN               
         MVC   BLDROW,=H'9'                                                     
         XC    BLDCOL,BLDCOL                                                    
         B     BLDF2                                                            
*                                                                               
BLDF1    SR    R0,R0                                                            
         LA    R2,BUYLAST                                                       
         OC    BLDTWAS,BLDTWAS     TEST FOR OPTIONAL USER START                 
         BZ    *+8                                                              
         L     R2,BLDTWAS                                                       
         CLI   0(R2),0             FIND THE CURRENT E-O-S                       
         BE    BLDF2                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-14                                                             
         SPACE                                                                  
* CALCULATE LENGTH NEEDED FOR FIELDS AND CHECK IF IT FITS ON                    
* CURRENT SCREEN LINE                                                           
*                                                                               
BLDF2    SR    R0,R0                                                            
         SR    RE,RE                                                            
         LA    R5,BLDLIST                                                       
*                                                                               
BLDF3    IC    R0,0(R5)            LEADING SPACES                               
         AR    RE,R0                                                            
         IC    R0,1(R5)            DATA LEN                                     
         AR    RE,R0                                                            
         LA    R0,2                UNPROTECTED FIELDS NEED 2 BYTES 0/H          
         TM    2(R5),X'20'         TEST PROTECTED                               
         BZ    *+6                                                              
         SR    R0,R0                                                            
         AR    RE,R0                                                            
         IC    R0,3(R5)            TRAILING SPACES                              
         AR    RE,R0                                                            
         LA    R5,8(R5)                                                         
         OC    0(4,R5),0(R5)       TEST FOR E-O-L                               
         BNZ   BLDF3               NO                                           
*                                                                               
         LH    R0,BLDCOL                                                        
         AR    R0,RE                                                            
         CH    R0,=H'80'           TEST IF FITS ON LINE                         
         BL    BLDF4               YES                                          
         SPACE                                                                  
* TRY IGNORING LAST TRAILING SPACES                                             
         SH    R5,=H'8'                                                         
         ZIC   R6,3(R5)                                                         
         SR    R0,R6                                                            
         CH    R0,=H'80'                                                        
         BNH   BLDF4               FITS ON LINE                                 
         SPACE                                                                  
* WILL NOT FIT THIS ROW - TRY NEXT                                              
         XC    BLDCOL,BLDCOL                                                    
         LH    R6,BLDROW                                                        
         LA    R6,1(R6)                                                         
         STH   R6,BLDROW                                                        
         CH    R6,=H'24'           TEST IF PAST E-O-S                           
         BNL   BLDFNEX             YES                                          
         SPACE                                                                  
* MAKE SURE DATA FITS IN TWA. RE HAS DATA LEN                                   
*                                                                               
BLDF4    LA    R5,BLDLIST                                                       
         LA    RE,8(RE)            ADD 8 FOR EACH FLDHDR                        
         LA    R5,8(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BNZ   *-14                                                             
         LA    RE,3(RE)            ADD 3 FOR GENCER INDICS                      
         AR    RE,R2                                                            
         C     RE,BLDTWAX          TEST AGAINST END OF TWA                      
         BNL   BLDFNEX             WILL NOT FIT                                 
         SPACE                                                                  
* NOW START BUILDING FIELDS FROM LIST ENTRIES                                   
*                                                                               
         LA    R5,BLDLIST          INITIALIZE LIST POINTER                      
*                                                                               
BLDF6    XC    0(8,R2),0(R2)       CLEAR FIELD HEADER AREA                      
         OI    6(R2),X'80'         SET XMIT BIT                                 
         OI    4(R2),X'20'         SET 'EDITED' FLAG                            
         ZIC   RE,1(R5)            GET FIELD LEN                                
         STC   RE,7(R2)            SET IT                                       
         OI    7(R2),X'80'         SET SPECIAL FLAG FOR XLTR                    
         BCTR  RE,0                                                             
         L     RF,4(R5)            GET DATA ADDR                                
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,SPACES                                                        
         EX    RE,BLDFMVC                                                       
         EX    RE,BLDFOC                                                        
         SPACE                                                                  
BLDF8    LA    RE,9(RE)            RESTORE TO LEN+8                             
         STC   RE,0(R2)            SET LEN+8                                    
         SPACE                                                                  
* BUILD OUTPUT FIELD ADDRESS                                                    
         LH    RE,BLDROW                                                        
         BCTR  RE,0                                                             
         MH    RE,=H'80'                                                        
         AH    RE,BLDCOL                                                        
         ZIC   R0,0(R5)            ADD LEADING SPACES                           
         AR    RE,R0                                                            
         TM    2(R5),X'20'         TEST PROTECTED                               
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STH   RE,2(R2)                                                         
         MVC   1(1,R2),2(R5)       SET PRO/UNP IND                              
         TM    2(R5),X'20'         TEST PROT                                    
         BO    *+8                                                              
         OI    1(R2),X'08'         SET HIGH INTENSITY FOR UNP FIELDS            
         ZIC   RE,0(R2)            GET FIELD LEN                                
         AR    R2,RE               POINT TO NEW END OF SCREEN                   
         SPACE                                                                  
* UPDATE COLUMN                                                                 
         ZIC   RE,0(R5)            LEADING SPACES                               
         ZIC   R0,1(R5)            DATA LENGTH                                  
         AR    RE,R0                                                            
         LA    R0,2                                                             
         TM    2(R5),X'20'         TEST PROTECTED                               
         BZ    *+6                                                              
         SR    R0,R0                                                            
         AR    RE,R0               + OVERHEAD                                   
         IC    R0,3(R5)            TRAILING SPACES                              
         AR    RE,R0                                                            
         AH    RE,BLDCOL           + PREVIOUS COL                               
         STH   RE,BLDCOL                                                        
*                                                                               
         LA    R5,8(R5)            NEXT FIELD ENTRY                             
         OC    0(4,R5),0(R5)       TEST FOR E-O-L                               
         BNZ   BLDF6               YES                                          
         XC    0(3,R2),0(R2)       SET E-O-S FLAG                               
         CR    RB,RB               SET CC =                                     
         B     BLDFX                                                            
         SPACE                                                                  
BLDFNEX  LTR   RB,RB               FORCE CC TO NOT =                            
         SPACE                                                                  
BLDFX    XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
BLDFMVC  MVC   8(0,R2),0(RF)                                                    
BLDFOC   OC    8(0,R2),SPACES                                                   
         EJECT                                                                  
* CENFLD - CENTRALIZE FIELDS ON SCREEN LINE                                     
*                                                                               
* ROUTINE ASSUMES THAT ONE OR MORE ITERATIONS OF FIELDS DESCRIBED               
* IN BLDLIST WILL COMPOSE SCREEN LINE.  LEADING AND TRAILING SPACES             
* BETWEEN EACH FIELD OR FIELD PAIR WILL BE CALCULATED TO SPREAD THE             
* FIELD(S) OUT EVENLY OVER LINE                                                 
*                                                                               
* ON EXIT, CC IS = IF FIELDS DO NOT FIT ON LINE, NEQ IF FIELDS FIT              
*                                                                               
CENFLD   SR    R0,R0                                                            
         SR    RE,RE               CLEAR ACCUM FOR TOTAL LEN NEEDED             
         SR    R3,R3               CLEAR ACCUM OF NUM OF ITERATIONS             
         LA    R5,BLDLIST                                                       
         SPACE                                                                  
* CALCULATE TOTAL LENGTH NEEDED FOR BLDLIST FIELDS                              
*                                                                               
CENFLD1  IC    R0,0(R5)            LEADING SPACES                               
         AR    RE,R0                                                            
         IC    R0,1(R5)            DATA LENGTH                                  
         AR    RE,R0                                                            
         LA    R0,2                2 BYTES OVERHEAD FOR UNPROTECTED             
         TM    2(R5),X'20'         TEST IF PROTECTED                            
         BZ    *+6                 NO                                           
         SR    R0,R0               YES                                          
         AR    RE,R0                                                            
         IC    R0,3(R5)            TRAILING SPACES                              
         AR    RE,R0                                                            
         LA    R5,8(R5)            NEXT BLDLIST ENTRY                           
         OC    0(4,R5),0(R5)       TEST FOR E-O-L                               
         BNZ   CENFLD1             NO                                           
         SPACE                                                                  
CENFLD2  CH    RE,=H'80'           TEST IF FITS ON LINE                         
         BL    CENFLD4             YES                                          
         SH    R5,=H'8'            BACK UP TO LAST ENTRY                        
         IC    R0,3(R5)            AND TRY REMOVING TRAILING SPACES             
         SR    RE,R0                                                            
         CH    RE,=H'80'                                                        
         BH    CENFLDX             WILL NOT FIT ON LINE                         
         SPACE                                                                  
* FIND NUM OF ITERATIONS OF BLDLIST ON LINE - THEN EXCESS                       
*                                                                               
CENFLD4  SR    R2,R2               PREPARE DIVIDEND                             
         LA    R3,80               LENGTH OF LINE                               
         DR    R2,RE               LEN OF LINE/LEN OF FIELDS                    
         LTR   R5,R2               EXCESS ON LINE                               
         BZ    CENFLDX             NONE                                         
         SR    R4,R4               PREPARE DIVIDEND                             
         DR    R4,R3               EXCESS BYTES/NUMBER OF ITERATIONS            
         LR    R4,R5               R5 CONTAINS EXCESS PER FIELD                 
         SRL   R4,1                R4 IS BYTES BEFORE FIELD(S)                  
         SR    R5,R4               REST IS BYTES AFTER FIELD(S)                 
         SPACE                                                                  
* UPDATE BLDLIST ENTRIES INSERTING EXTRA LEADING AND TRAILING SPACES            
*                                                                               
CENFLD6  LA    R1,BLDLIST                                                       
         IC    R0,0(R1)            LEADING SPACES                               
         AR    R0,R4               ADD IN BLANKS TO SPACE IT OUT                
         STC   R0,0(R1)                                                         
         LA    R1,8(R1)            NEXT FIELD                                   
         OC    0(4,R1),0(R1)       TEST FOR E-O-L                               
         BNZ   *-10                NO                                           
         SH    R1,=H'8'                                                         
         IC    R0,3(R1)            TRAILING SPACES                              
         AR    R0,R5                                                            
         STC   R0,3(R1)                                                         
         SPACE                                                                  
CENFLDX  LTR   R3,R3               SET CC FOR EXIT                              
         B     EXXMOD                                                           
         EJECT                                                                  
*--READ NAD DEFINITION RECORD THROUGH THE PROGRAM RECORD                        
*--AND FILTER OUT ALL NAD DEMOS THAT ARE NOT DEFINED ON                         
*--THE NAD DEFINITION RECORD                                                    
* SUB-ROUTINE TO READ NADDEF RECORD AND RETRIEVE THE NAD DEMOS                  
*                                                                               
GETNADS  L     R3,APROGREC                                                      
         USING NPGRECD,R3                                                       
*                                                                               
         XC    HUTBLK,HUTBLK                                                    
         CLC   NPGKTYP(2),=XL2'0D20'   WAS PROGRAM KEY READ                     
         BNE   GETNDEX             NO EXIT                                      
*                                                                               
         USING NPGEL03,R2                                                       
         LA    R2,NPGMAINL                                                      
         ZIC   RE,1(R2)            TEST IF NAD ELEMENT FOUND                    
         AR    R2,RE               NO                                           
         CLI   0(R2),X'03'                                                      
         BNE   GETND25                                                          
         OC    NPGNADDM,NPGNADDM   NO NAD DEF RECORD                            
         BZ    GETND25             BYPASS ALL NADS ON ESTIMATE                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0D16'                                                
         MVC   KEY+2(1),AGYMED                                                  
         MVC   KEY+3(6),NPGNADDM   NAD DEFINITION CODE                          
         MVC   KEYSAVE(13),KEY                                                  
         DROP  R2,R3                                                            
*                                                                               
         GOTO1 AIO,DMCB,SPT+DIR+HIGH    RE-READ AND REPLACE                     
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETNDEX                                                          
         GOTO1 (RF),(R1),SPT+FILE+GET,AIOAREA4                                  
*                                                                               
         USING NNDRECD,R3                                                       
         L     R3,AIOAREA4                                                      
         LA    R2,NNDMAINL                                                      
         XC    HUTBLK,HUTBLK                                                    
         LA    RE,HUTBLK                                                        
*                                                                               
GETND20  ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BE    GETND25                                                          
         CLI   0(R2),X'DD'                                                      
         BNE   GETND20                                                          
         MVC   0(3,RE),3(R2)                                                    
         LA    RE,3(RE)                                                         
         B     GETND20                                                          
*                                                                               
GETND25  LA    RE,ESTDEMSE                                                      
         ZIC   RF,ESTNDEMS                                                      
         LA    R4,FLD                                                           
         SR    R1,R1                                                            
*                                                                               
GETND30  CLI   0(RE),0             IS DEMO NAD                                  
         BE    GETND50             NO BYPASS                                    
         CLI   0(RE),171           IS DEMO TVQ                                  
         BE    GETND50             YES BYPASS NAD CHECK                         
         LA    R2,HUTBLK                                                        
         LA    R3,40                                                            
GETND35  OC    0(3,R2),0(R2)                                                    
         BZ    GETND55                                                          
         CLC   0(1,RE),0(R2)                                                    
         BNE   GETND40                                                          
         CLC   2(1,RE),2(R2)                                                    
         BE    GETND50                                                          
GETND40  LA    R2,3(R2)                                                         
         BCT   R3,GETND35                                                       
         B     GETND55                                                          
GETND50  MVC   0(3,R4),0(RE)                                                    
         LA    R4,3(R4)                                                         
         LA    R1,1(R1)                                                         
GETND55  LA    RE,3(RE)            GET NEXT ESTLIST DEMO                        
         BCT   RF,GETND30                                                       
         STC   R1,ESTNDEMS                                                      
*                                                                               
         XC    ESTDEMSE,ESTDEMSE                                                
         XC    ESTDEMS,ESTDEMS                                                  
         ZIC   RF,ESTNDEMS                                                      
         LTR   RF,RF               ANY DEMOS QUALIFY                            
         BZ    GETNDEX             NO EXIT                                      
         LA    R1,ESTDEMSE                                                      
         LA    R2,FLD                                                           
*                                                                               
GETND60  MVC   0(3,R1),0(R2)                                                    
         LA    R1,3(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   RF,GETND60                                                       
*                                                                               
         LA    RE,DBLOCKA                                                       
         L     RF,=F'256'                                                       
         XCEF                                                                   
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         GOTO1 VDEMOCON,DMCB,(50,ESTDEMSE),('DEMOCON_17',ESTDEMS),     X        
               (C'S',DBLOCK),ESTDEMPL                                           
         DROP  RF                                                               
*                                                                               
         SPACE 1                                                                
GETNDEX  XC    HUTBLK,HUTBLK                                                    
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* CKREASN - CHECKS THAT REASON CODE IS VALID                                    
*                                                                               
* AT ENTRY P1 CONTAINS A(4 BYTE REASON CODE)                                    
*                                                                               
CKREASN  L     RE,0(R1)            POINT TO DATE                                
         ICM   R0,15,0(RE)                                                      
         LA    R4,KEY                                                           
         USING RSNRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   RSNKTYPE,=X'0D77'                                                
         MVC   RSNKAGY,AGENCY                                                   
         MVI   RSNKMED,C'N'                                                     
         STCM  R0,15,RSNKCODE                                                   
         OC    RSNKCODE,SPACES                                                  
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
                                                                                
         CLC   KEY(9),KEYSAVE      TEST IF SAME RECORD APPLIES                  
         BE    CKREASNX            YES                                          
         MVI   FERN,INVREASN                                                    
         B     ERROR                                                            
CKREASNX B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* BLDTVQ  - BUILDS DEFAULT TVQ ELEMENTS                                         
*                                                                               
* AT ENTRY P1 CONTAINS A(UNITREC)                                               
*                                                                               
BLDTVQ   L     R3,0(R1)             A(UNIT RECORD)                              
         USING NURECD,R3                                                        
         LA    RE,ESTDEMSE                                                      
         LA    RF,20                                                            
*                                                                               
BLTVQ050 CLI   0(RE),171            CHECK TVQ DEMO                              
         BE    BLTVQ100                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,BLTVQ050                                                      
         B     BLTVQEX                                                          
*                                                                               
* CHECK IF ESTIMATED BOOK ALREADY LOADED                                        
BLTVQ100 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'60',0(R3)),(1,=C'J')               
         CLI   12(R1),0                                                         
         BE    BLTVQ200                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(3),=X'6007D1'                                               
*                                                                               
*        GET LATEST BOOK VIA DEMAND                                             
*                                                                               
         LA    RE,DBLOCKA                                                       
         L     RF,=F'256'                                                       
         XCEF                                                                   
*                                                                               
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELPRG,=X'0001'                                                
*                                                                               
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBFUNCT,DBGETNTI                                                 
*                                                                               
         MVC   DBSELSTA(4),NUKNET      STATION TYPE                             
*                                                                               
         MVI   DBSELSTA+4,C'N'                                                  
         CLI   NBSTSTAT,C'C'                                                    
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'C'     CABLE                                        
*                                                                               
         MVI   DBBTYPE,C'U'                                                     
         MVI   DBSELDUR,X'FF'      ALL DURATIONS                                
         MVI   DBPRGDUR,C'Y'                                                    
         MVI   DBBEST,C'A'                                                      
         XC    DBSELBK,DBSELBK     BK IS HI, LK UP LATEST                       
*                                                                               
         L     RE,AIOAREA3                                                      
         ST    RE,DBAREC                                                        
         L     RF,=F'1000'                                                      
         XCEF                                                                   
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CDEMAND,DMCB2,DBLOCKA,0                                          
         DROP  R4                                                               
*                                                                               
         LA    R4,DBLOCKA                                                       
         USING DBLOCK,R4                                                        
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),DBACTBK                                                   
         MVI   DUB+2,X'0F'         DEFAULT TO YYMM15                            
         DROP  R4                                                               
*                                                                               
         GOTO1 VDATCON,DMCB2,(3,DUB),(0,DUB)                                    
*                                                                               
         GOTO1 VNETWEEK,DMCB2,DUB,VGETDAY,VADDAY                                
*        MVI   1(R2),4                                                          
         MVC   WORK+3(1),4(R1)                                                  
         MVC   WORK+4(1),8(R1)                                                  
         GOTO1 VDATCON,(R1),(0,DUB),(2,WORK+5)                                  
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'60',0(R3)),WORK,0                  
*                                                                               
BLTVQ200 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'60',0(R3)),(1,=C'K')               
         CLI   12(R1),0                                                         
         BE    BLTVQEX                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(3),=X'6007D2'                                               
*                                                                               
         MVC   WORK+5(2),NUKDATE                                                
         GOTO1 VDATCON,(R1),(2,NUKDATE),(0,DUB)                                 
         GOTO1 VNETWEEK,DMCB2,DUB,VGETDAY,VADDAY                                
         MVC   WORK+3(1),4(R1)                                                  
         MVC   WORK+4(1),8(R1)                                                  
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'60',0(R3)),WORK,0                  
*                                                                               
BLTVQEX  B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* GENERATE A TUNAROUND REQUEST ON BU CHANGES                                    
*                                                                               
*                                                                               
BLDRQST  CLC   CLIFPGRP,SPACES                                                  
         BNH   BLDRQEX                                                          
         CLC   =C'G7',AGYALPH                                                   
         BNE   BLDRQ4                                                           
         TM    ESTFLAG1,EF1REQ     TEST ESTIMATE REQUESTABLE                    
         BZ    BLDRQEX                                                          
*                                                                               
BLDRQ4   XC    HUTBLK(26),HUTBLK                                                
         MVC   HUTBLK+26(80),SPACES                                             
         MVI   HUTBLK+14,106                                                    
         LA    R4,HUTBLK+26                                                     
         MVC   0(2,R4),=C'RF'                                                   
         MVC   2(2,R4),AGYALPH                                                  
         MVI   4(R4),C'N'                                                       
         MVC   5(3,R4),CLI                                                      
         MVC   11(3,R4),=C'POL'                                                 
         MVC   18(4,R4),NET                                                     
         MVI   22(R4),C'N'                                                      
         EDIT  (1,EST),(3,23(R4)),FILL=0                                        
         MVC   49(8,R4),CLIFPGRP                                                
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',HUTBLK,HUTBLK                
*                                                                               
BLDRQEX  B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
* HNDLDPT - DOES ALL USER DAYPART HANDLING                                      
*                                                                               
* AT ENTRY P1 CONTAINS BYTE 1 = 1(DAYPART RETURN)                               
*                      BYTE 1 = 0(DAYPART VALIDATION)                           
*                                                                               
*                      A(OF DAYPART CODE)                                       
* RETURN IF KEY = KAESAVE DAYPART VALID                                         
*                                                                               
* ON RETURN                                                                     
* FLD   = 1 OR 2 BYTE DAYPART CODE                                              
* FLD+2 = 14 BYTE DAYPART DESCRIPTION                                           
*                                                                               
HNDLDPT  L     R2,0(R1)                                                         
         USING DPTRECD,R4                                                       
*                                                                               
         CLI   0(R1),1                                                          
         BE    HNDPT200             DO LOOKUP                                   
*                                                                               
*  VALIDATE A DAYPART                                                           
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,AGYMED                                                   
*                                                                               
HNDPT010 GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         B     HNDPT040                                                         
HNDPT020 GOTO1 AIO,DMCB,UNT+DIR+SEQ                                             
                                                                                
HNDPT040 CLC   KEY(5),KEYSAVE      CHECK UP TO CLIENT                           
         BNE   HNDPT100            YES                                          
         CLC   NDPTDPTA,0(R2)      CHECK FOR MATCH ON CODE                      
         BNE   HNDPT020            GET NEXT RECORD                              
*  MOVE DATA OUT AND EXIT                                                       
         B     HNDPTEX                                                          
*                                                                               
* CHECK KEY FOR AGENCY OR CLIENT LEVEL CHECK                                    
* IF AGENCY LEVEL RESET KEY MOVE CLIENT CODE IN RESTART SEARCH                  
* IF CLIENT LEVEL EXIT ROUTINE DEMO WAS INVALID                                 
*                                                                               
HNDPT100 OC    KEYSAVE+3(2),KEYSAVE+3                                           
         BNZ   HNDPTBEX                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),KEYSAVE                                                   
         MVC   KEY+3(2),CLIPK                                                   
         B     HNDPT010                                                         
*                                                                               
* FOLLOWING ROUTINE FINDS THE 2 CHARACTER CODE AND EXPANSION                    
*                                                                               
*                                                                               
HNDPT200 LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,AGYMED                                                   
         CLI   0(R2),127            CHECK CLIENT LEVEL DAYPART                  
         BH    *+10                                                             
         MVC   NDPTCLT,CLIPK                                                    
         MVC   NDPTDPTE,0(R2)                                                   
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BNE   HNDPTBEX                                                         
*                                                                               
HNDPTEX  MVC   KEYSAVE,KEY          ALL NEEDED DISP. FIELDS IN KEYSAVE          
HNDPTBEX B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* CHKAIR  - SETS DEMO PRECISSION FOR THE AIRING VENDOR                          
*                                                                               
* AT ENTRY P1 CONTAINS A(UNITREC)                                               
*                                                                               
CHKAIR   L     R3,0(R1)             A(UNIT RECORD)                              
         USING NURECD,R3                                                        
*                                                                               
* RESET DEFAULT VALUE                                                           
*                                                                               
         MVC   DEMPREC(14),COPREC  CABLE PRECISSION                             
         CLI   DEFDPREC,C'C'                                                    
         BE    CHKAIR10                                                         
         MVC   DEMPREC(14),NSPREC  NETWORK PRECISSION                           
*                                                                               
* CHECK IF AIRING STATION IS SET                                                
*                                                                               
CHKAIR10 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'60',0(R3)),(1,=C'V')               
         CLI   12(R1),0                                                         
         BNE   CHKAIR30                                                         
         L     RE,12(R1)                                                        
*  SET THE DEMO PRECISSION                                                      
         CLI   12(RE),C'C'                                                      
         BE    CHKAIR20                                                         
         CLI   12(RE),C'D'                                                      
         BE    CHKAIR20                                                         
         CLI   12(RE),C'O'                                                      
         BNE   *+18                                                             
CHKAIR20 MVC   DEMPREC(14),COPREC                                               
         MVI   NBPREOPT,C'Y'                                                    
         B     CHKAIR30                                                         
         MVC   DEMPREC(14),NSPREC                                               
         MVI   NBPREOPT,C'N'                                                    
*                                                                               
CHKAIR30 DS    0H                                                               
         TM    DEMFLAG,D2PRE                                                    
         BZ    CHKAIREX                                                         
*                                                                               
         MVC   DEMPREC(14),D2PRECNS     INITIALIZE TO NETWORK/SYND.             
         CLI   NBPOSTYP,C'C'                                                    
         BE    CHKAIR40                                                         
         CLI   NBPOSTYP,C'D'                                                    
         BE    CHKAIR40                                                         
         CLI   NBPOSTYP,C'O'                                                    
         BNE   CHKAIREX                                                         
*                                                                               
CHKAIR40 MVC   DEMPREC(14),D2PRECCO     THEN IT'S CABLE/OTHER                   
*                                                                               
CHKAIREX B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* GETFLT  - GETS THE FLIGHT INFO FOR THE UNIT                                   
*                                                                               
* AT ENTRY P1 CONTAINS A(BUY RECORD)                                            
*                                                                               
* ON RETURN            A(11 BYTE RETURN AREA FLIGHT NUMBER)                     
*                                                                               
* WARNING THIS ROUTINE USES THE COMMON STORAGE FIELD "WORK"                     
*                                                                               
*                                                                               
GETFLT   LM    R3,R4,0(R1)             A(UNIT RECORD)                           
         L     R5,ATIA             R5 POINTS TO BUYVALS                         
         USING NURECD,R3                                                        
         XC    0(11,R4),0(R4)                                                   
*                                                                               
* CHECK IF FLIGHT OVERRIDE ON UNIT                                              
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'60',0(R3)),(1,=C'1')               
         CLI   12(R1),0                                                         
         BNE   GETFL100                                                         
         L     RE,12(R1)                                                        
         MVC   0(10,R4),3(RE)       MOVE OVERRIDE FLIGHT NUMBER OUT             
         B     GETFLEX                                                          
*                                                                               
*  BUILD FLIGHT KEY                                                             
*                                                                               
*  GET DAYPART INFORMATION                                                      
GETFL100 MVC   0(1,R4),NUKDP        SAVE UNIT DAYPART                           
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'60',0(R3)),(1,=C'D')               
         CLI   12(R1),0                                                         
         BE    GETFL105                                                         
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'60',0(R3)),(1,=C'U')               
         CLI   12(R1),0                                                         
         BNE   GETFL110                                                         
GETFL105 L     R2,12(R1)                                                        
         GOTO1 VALDAYPT,DMCB,(0,3(R2))                                          
         BE    *+6                                                              
         DC    H'0'                 DAYPART RECORD NOT FOUND IMPOSSIBLE         
         MVI   NBFUNCT,NBFRDHI                                                  
         MVC   0(1,R4),KEY+5        MOVE SUB DAYPART EQUATE                     
****     MVC   KEY(20),0(R3)        REPOSITION UNIT POINTER                     
****     GOTO1 AIO,DMCB,UNT+DIR+READ                                            
****     CLC   KEY(20),KEYSAVE                                                  
****     BE    *+6                                                              
****     DC    H'0'                                                             
*                                                                               
*  GET PRODUCT FROM 19 ELEMENT                                                  
GETFL110 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',0(R3)),0                       
         CLI   12(R1),0                                                         
         BNE   GETFL350                                                         
         L     RE,12(R1)                                                        
         MVC   1(3,R4),3(RE)                                                    
*                                                                               
*  GET FLIGHT TYPE                                                              
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',0(R3)),0                       
         CLI   12(R1),0                                                         
         BNE   GETFL350                                                         
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
*                                                                               
*  IF D RADIO FLIGHT TYPE MUST BE "D"                                           
         MVI   4(R4),C'D'                                                       
         CLI   NUSTATYP,C'D'                                                    
         BE    GETFL150                                                         
*                                                                               
*  IF N NETWORK FLIGHT DEFAULTS TO NULLS UNLESS                                 
*  FIELD NUSDSBMD = "I" OR "U"                                                  
         CLI   NUSTATYP,C'N'                                                    
         BNE   GETFL120                                                         
         MVC   4(1,R4),NUSDSBMD                                                 
         CLI   NUSDSBMD,C'I'                                                    
         BE    GETFL150                                                         
         CLI   NUSDSBMD,C'U'                                                    
         BE    GETFL150                                                         
         MVI   4(R4),0                                                          
         B     GETFL150                                                         
*                                                                               
*  IF C CABLE FLIGHT DEFAULTS TO "C" UNLESS                                     
*  FIELD NUSDSBMD = "V", "B", "M", "G" OR "T"                                   
GETFL120 CLI   NUSTATYP,C'C'                                                    
         BNE   GETFL130                                                         
         MVC   4(1,R4),NUSDSBMD                                                 
         CLI   NUSDSBMD,C'T'                                                    
         BE    GETFL150                                                         
         CLI   NUSDSBMD,C'V'                                                    
         BE    GETFL150                                                         
         CLI   NUSDSBMD,C'B'                                                    
         BE    GETFL150                                                         
         CLI   NUSDSBMD,C'G'                                                    
         BE    GETFL150                                                         
         CLI   NUSDSBMD,C'M'                                                    
         BE    GETFL150                                                         
         MVI   4(R4),C'C'                                                       
         B     GETFL150                                                         
*                                                                               
*  IF MEDIA S/O FLIGHT TYPE IS SET TO NULLS                                     
GETFL130 MVI   4(R4),0                                                          
         CLI   NUSTATYP,C'S'                                                    
         BE    GETFL150                                                         
         CLI   NUSTATYP,C'O'                                                    
         BE    GETFL150                                                         
         DROP  RE                                                               
*                                                                               
*  BUILD THE KEY (FOR DAYPART LEVEL FLIGHT RECORD)                              
GETFL150 XC    KEY,KEY                                                          
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING WFLIGHTD,RE                                                      
         MVI   WP1TYP,WP1TYPQ                                                   
         MVI   WP1STYP,WP1STYPQ                                                 
         MVC   WP1AM,NUKAM          AGENCY/MEDIA                                
         MVC   WP1CLT,NUKCLT        CLIENT                                      
         MVC   WP1PRD,1(R4)         PRODUCT                                     
         MVC   WP1FTYPE,4(R4)       FLIGHT TYPE                                 
         MVC   WP1DPT,0(R4)         DAYPART                                     
         MVC   WP1END,NUKDATE       END DATE                                    
         MVC   KEY,WORK                                                         
*                                                                               
         GOTO1 AIO,DMCB,XSP+DIR+HIGH                                            
         B     GETFL180                                                         
GETFL170 GOTO1 AIO,DMCB,XSP+DIR+SEQ                                             
GETFL180 LA    RE,KEY                                                           
         TM    WFKCNTRL,WFKINACT    CHECK IF KEY IS ACTIVE                      
         BNZ   GETFL170                                                         
         CLC   KEY(WP1END-WP1TYP),WORK                                          
         BNE   GETFL200                                                         
*  CHECK IF UNIT FALL WITHIN KEY DATE RANGE                                     
         LA     RE,KEY                                                          
         CLC    NUKDATE,WP1STD                                                  
         BL     GETFL170                                                        
         CLC    NUKDATE,WP1END                                                  
         BH     GETFL170                                                        
         B      GETFL500                                                        
*                                                                               
*  BUILD THE KEY (THE DEFAULT RECORD)                                           
GETFL200 LA    RE,WORK                                                          
         XC    KEY,KEY                                                          
         MVI   WP1DPT,0                                                         
         MVC   KEY(WP1END-WP1TYP),WORK                                          
*                                                                               
         GOTO1 AIO,DMCB,XSP+DIR+HIGH                                            
         B     GETFL260                                                         
GETFL240 GOTO1 AIO,DMCB,XSP+DIR+SEQ                                             
GETFL260 LA    RE,KEY                                                           
         TM    WFKCNTRL,WFKINACT    CHECK IF KEY IS ACTIVE                      
         BNZ   GETFL240                                                         
         CLC   KEY(WP1END-WP1TYP),WORK                                          
         BE    GETFL400                                                         
*  NO FLIGHT ATTACHED TO THIS UNIT                                              
GETFL350 MVC   0(11,R4),SPACES      CLEAR THE OUTPUT AREA                       
         B     GETFLEX                                                          
*  CHECK IF UNIT FALL WITHIN KEY DATE RANGE                                     
GETFL400 LA     RE,KEY                                                          
         CLC    NUKDATE,WP1STD                                                  
         BL     GETFL240                                                        
         CLC    NUKDATE,WP1END                                                  
         BH     GETFL240                                                        
*  OUTPUT THE FLIGHT NUMBER                                                     
GETFL500 MVI    0(R4),C'#'                                                      
         MVC    1(10,R4),WP1WFID                                                
*                                                                               
GETFLEX  B     EXXMOD                                                           
         DROP  R3,RE                                                            
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
CHKSEC   DS    0H                                                               
         BRAS  RE,CHKSECU                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
*=========================================================                      
* CALL ENDEDEMC TO DECODE OR ENCODE DEMO STRING                                 
*=========================================================                      
DEDEND   DS    0H                                                               
         BRAS  RE,ENDEDEMC                                                      
         B     EXXMOD                                                           
         EJECT                                                                  
********************************************************************            
*    UPDATE BILL KEYS WHEN UNIT RECORD KEY CHANGE/DELETE                        
*    ON INPUT  PARAM1 - C'D' (UNIT WAS DELETED SO DELETE BILLING)               
*                       A(UNIT KEY)                                             
*              PARAM2 - A(OLD UNIT KEY - ACTION CHANGE)                         
*              PARAM3 - A(AIOAREA FOR BILLING RECORDS)                          
*              PARAM4 - A(BILL KEY AREA FOR DATAMGR)                            
********************************************************************            
CHGBILL  DS    0H                                                               
         BRAS  RE,CHGBIL                                                        
         B     EXXMOD                                                           
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FVAL - FIELD VALIDATION AND SCANNING ROUTINE                                  
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER)                                                
*        FMAX  = MAXIMUM SCAN LENGTH (OPTIONALLY SET BY USER)                   
*        FLAST = A(LAST STRING) SET BY FVAL                                     
*              = ZERO (FORCES EDIT TO START AT FADDR+8)                         
*        FLEN  = LENGTH OF LAST STRING - SET BY FVAL                            
*              = ZERO TO FORCE EDIT TO START AT FLAST                           
*        FTERM = LIST OF UP TO 6 SCAN TERMINATORS ENDED BY X'00'                
*                                                                               
* ON EXIT                                                                       
*        FLDH  = FIELD HEADER BUILT BY FVAL - CONTAINS DATA LENGTH              
*                AND VALIDITY BITS                                              
*        FLD   = EXTRACTED DATA STRING IN SPACE FILLED FIELD                    
*        FSTOP = STOP CHARACTER FOUND BY FVAL OR X'FF' FOR NO MORE DATA         
*        DUB   = CONTAINS PACKED VALUE OF DATA STRING FOR NUMERIC FIELD         
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
*        R1    = 0 FOR NO PARAMETER LIST                                        
*                                                                               
FVAL     NTR1  BASE=BASE1                                                       
         L     RA,BASE2                                                         
         L     R7,BASE3                                                         
         XC    FLDH,FLDH           CLEAR OUTPUT FIELD HEADER                    
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL OUTPUT FIELD WITH SPACES                
         MVI   FLDH,L'FLDH+L'FLD   SET DUMMY HEADER LENGTH                      
         MVI   FSTOP,X'FF'         SET FSTOP TO NO DATA FOUND                   
         L     R2,FADDR                                                         
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'8'            R3 CONTAINS FIELD DATA LENGTH                
         LA    R2,8(R2)            POINT TO DATA START                          
         OC    FLAST,FLAST         TEST FOR LAST STRING                         
         BNZ   FVAL2                                                            
*                                                                               
* CODE BELOW TO FVAL8 ASSUMES -                                                 
* R1 POINTS TO SCAN INPUT  R2 POINTS TO SCREEN FIELD START                      
* R3 CONTAINS BYTE REMAINING TO BE SCANNED                                      
*                                                                               
FVAL1    STCM  R2,7,FLAST          SAVE SCAN START POINT                        
         LR    R1,R2               SET R1 AS SCAN INPUT POINTER                 
         MVI   FLEN,0              CLEAR LAST LENGTH TO BE SAFE                 
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         B     FVAL4                                                            
*                                                                               
FVAL2    SR    R1,R1                                                            
         ICM   R1,7,FLAST          SET R1 TO POINT TO SCAN START                
         ZIC   R0,FLEN             LENGTH OF LAST STRING                        
         AR    R1,R0               POINT TO LAST STOP CHARACTER                 
         CLI   FLEN,0              TEST IF RE-EDITING                           
         BE    *+8                 YES-NO NEED TO JUMP OVER STOP CHAR.          
         LA    R1,1(R1)            INCREMENT LENGTH FOR STOP CHARACTER          
         LR    R0,R1               START POINT FOR SCAN                         
         SR    R0,R2               BYTES ALREADY SCANNED                        
         SR    R3,R0               BYTES LEFT TO SCAN                           
         BNP   FVALX               NOTHING TO SCAN - EXIT                       
         CLI   FMAX,0              TEST FOR USER SCAN LIMIT                     
         BE    *+8                 NO                                           
         IC    R3,FMAX             YES-USE IT IN PLACE OF DERIVED LIMIT         
         STCM  R1,7,FLAST          SAVE SCAN START                              
         MVI   FLEN,0              CLEAR LAST LENGTH                            
*                                                                               
FVAL4    LA    R0,L'FTERM                                                       
         LA    RE,FTERM            POINT AT SCAN TERMINATORS                    
         SPACE 1                                                                
FVAL5    CLC   FTERM(5),=CL5'FIELD'  SCAN WHOLE FIELD                           
         BE    FVAL6                                                            
         CLI   0(RE),X'00'         TEST FOR END-OF-LIST                         
         BE    FVAL6                                                            
         CLC   0(1,R1),0(RE)       TEST FOR TERMINATOR                          
         BE    FVAL7               FOUND ONE                                    
         LA    RE,1(RE)            NEXT LIST ENTRY                              
         BCT   R0,FVAL5                                                         
*                                                                               
FVAL6    LA    R1,1(R1)            NEXT DATA BYTE                               
         BCT   R3,FVAL4                                                         
         B     FVAL8               SEARCH WAS FRUITLESS                         
*                                                                               
FVAL7    MVC   FSTOP,0(R1)         SET STOP CHARACTER                           
*                                                                               
FVAL8    LR    R3,R1               COMPUTE DATA LENGTH                          
         SR    RE,RE                                                            
         ICM   RE,7,FLAST                                                       
         SR    R3,RE                                                            
         BZ    FVALX               ONLY FOUND A TERMINATOR                      
         STC   R3,FLEN                                                          
         STC   R3,FLDH+5                                                        
         BCTR  R3,0                SET TO EXECUTE                               
         EX    R3,*+8              EXTRACT DATA STRING                          
         B     FVAL10                                                           
         MVC   FLD(0),0(RE)                                                     
*                                                                               
FVAL10   LA    RE,FLD(R3)          ADJUST LENGTH FOR TRAILING BLANKS            
         LA    R3,1(R3)            COUNTER                                      
         LR    R0,R3                                                            
*                                                                               
FVAL11   CLI   0(RE),0             TEST FOR TRAILING ZERO                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          CHANGE IT TO A BLANK                         
         CLI   0(RE),C' '          TEST FOR A BLANK                             
         BNE   *+10                NO                                           
         BCTR  RE,0                YES-BACK UP FIELD POINTER                    
         BCT   R0,FVAL11                                                        
         STC   R0,FLDH+5                                                        
         LTR   R0,R0               TEST FOR ZERO REAL LENGTH                    
         BZ    FVALX               EXIT FOR EMPTY FIELD                         
         LR    R3,R0               SET REAL DATA LENGTH                         
         LA    RE,FLD              POINT TO START OF DATA                       
         MVI   FLDH+4,X'0C'        VALID NUMERIC AND ALPHA DATA                 
*                                                                               
FVAL12   CLI   0(RE),C'A'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'I'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'J'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'R'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'S'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'Z'                                                       
         BNH   FVAL14              VALID ALPHA                                  
*                                                                               
FVAL13   NI    FLDH+4,X'FF'-X'04'  NOT ALPHA                                    
*                                                                               
FVAL14   CLI   0(RE),C'0'          TEST IF NUMERIC                              
         BL    *+12                                                             
         CLI   0(RE),C'9'                                                       
         BNH   FVAL15                                                           
         NI    FLDH+4,X'FF'-X'08'  NOT NUMERIC                                  
*                                                                               
FVAL15   LA    RE,1(RE)            NEXT BYTE IN DATA STRING                     
         BCT   R3,FVAL12                                                        
*                                                                               
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BZ    FVALX                                                            
         CLI   FLDH+5,15                                                        
         BNH   *+12                                                             
         NI    FLDH+4,X'FF'-X'08'                                               
         B     FVALX                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     FVALX                                                            
         PACK  DUB,FLD(0)          EXECUTED                                     
*                                                                               
FVALX    MVI   FMAX,0              ALWAYS CLEARED BY FVAL                       
         B     EXXMOD                                                           
         EJECT                                                                  
* I/O CONTROLLER                                                                
*     ON ENTRY   KEY      = CONTAINS KEY FOR READ                               
*                NDXDA    = CONTAINS DISK ADDRESS FOR D/A FILE READ             
*                                                                               
*     PARAMETERS                                                                
*                P1       = I/O MASK - COMMAND/FILE/DIRECTORY OR                
*                           FILE/CONTROL SETTINGS                               
*                P2       = BYTES 1-3 - A(I/O AREA) FOR FILE READ               
*                                                                               
*     AFTER I/O                                                                 
*                IOFLAG   = 0 OR ERROR SETTING                                  
*                KEYSAVE  = KEY PASSED TO ROUTINE                               
*                KEY      = CONTAINS KEY RETURNED BY DATAMGR                    
*                AIOAREA  = A(I/O AREA) FOR RECORD                              
*                                                                               
IO       NTR1  BASE=BASE1,WORK=(RC,IOWORKX-IOWORKD)                             
         L     RA,BASE2            SECOND BASE REGISTER                         
         L     R7,BASE3            THIRD BASE REGISTER                          
         USING IOWORKD,RC          LOCAL STORAGE                                
         LM    R2,R3,0(R1)         I/O MASK AND I/O AREA                        
         STC   R2,IOWORK1          LOW ORDER BYTE OF I/O MASK                   
         MVC   IOWORK2,IOWORK1     SAVE LOW ORDER BYTE                          
         STCM  R2,2,IOWORK3        SAVE THIRD BYTE                              
         NI    IOWORK2,X'0F'       ISOLATE COMMAND NUMBER                       
         BAS   RE,SETCTL           SET CONTROL BIT                              
         ZIC   R1,IOWORK2                                                       
         SLL   R1,3                MULTIPLY BY 8 TO DEVELOP INDEX               
         LAY   RE,CMNDTAB-L'CMNDTAB(R1) INDEX INTO COMMAND TABLE                
         MVC   IOCMND,0(RE)        EXTRACT COMMAND NAME                         
         SPACE 1                                                                
IO2      MVC   IOWORK2,IOWORK1     REFRESH FLAG VALUES                          
         NI    IOWORK2,X'70'       ISOLATE FILE NUMBER                          
         L     RE,=A(FILTAB)                                                    
         A     RE,BRELO                                                         
         ZIC   R1,IOWORK2          FILE NUMBER                                  
         SRL   R1,4                DIVIDE BY 8 TO DEVELOP INDEX                 
         LA    R0,L'FILTAB                                                      
         SR    RE,R0                                                            
         MR    R0,R0                                                            
         AR    RE,R1                                                            
******** LA    RE,FILTAB-L'FILTAB(R1)                                           
         MVC   IODIR,0(RE)         EXTRACT DIRECTORY NAME                       
         MVC   IOFILE,7(RE)        FILE NAME                                    
         MVC   IOVALS,14(RE)                                                    
         BAS   RE,SETSTAT          SET LAST STATUS LOG                          
         ZIC   R1,IOWORK1          TEST IOFLAG VS. EXCEPTION VALS               
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    IOEXCPT,0                                                        
         BZ    *+6                                                              
         DC    H'0'                YES-BLOW UP - CHECK IOFLAG                   
         TM    IOWORK3,X'01'       TEST FOR PASS DELETES                        
         BZ    *+8                 NO                                           
         OI    DMINBITS,X'08'                                                   
         TM    IOWORK3,X'02'       TEST FOR READ FOR UPDATE                     
         BZ    *+8                 NO                                           
         OI    DMINBITS,X'80'                                                   
         TM    IOWORK1,DIR         TEST FOR DIRECTORY READ                      
         BZ    IO6                                                              
         SPACE 1                                                                
IO4      MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,IOPARM,(DMINBITS,IOCMND),IODIR,KEY,KEY                  
         LA    RE,KEY                                                           
         ZIC   R0,IODADSP                                                       
         AR    RE,R0               POINT TO DISK ADDRESS                        
         MVC   NDXDA,0(RE)                                                      
         MVC   IOFLAG,8(R1)                                                     
         B     IOX                                                              
         SPACE 1                                                                
IO6      ST    R3,AIOAREA                                                       
         LA    R2,NDXDA            POINT AT DISK ADDRESS                        
         TM    IOEXCPT,DIR         TEST FOR FILE W DIRECTORY                    
         BZ    IO7                                                              
         LA    R2,KEY              FOR IS FILE, POINT TO KEY                    
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
IO7      CLC   =C'PUTREC',IOCMND                                                
         JNE   *+8                                                              
         BRAS  RE,CHKPREM          CHECK PRE-EMPTED UNIT                        
*                                                                               
         GOTO1 VDATAMGR,IOPARM,(DMINBITS,IOCMND),IOFILE,(R2),AIOAREA,  X        
               DMWORK                                                           
         MVC   IOFLAG,8(R1)                                                     
         TM    IOEXCPT,DIR         TEST FOR IS FILE                             
         BZ    IOX                 NO                                           
         L     RF,AIOAREA                                                       
         ZIC   R1,IOKEYL                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     IOX                                                              
         MVC   KEY(0),0(RF)        EXTRACT KEY FROM RECORD                      
         SPACE 1                                                                
IOX      MVI   DMINBITS,0                                                       
         TM    IOFLAG,X'FD'        TEST FOR DATAMGR ERROR                       
         BZ    IOXX                NONE                                         
         MVI   FERN,0                                                           
         TM    IOFLAG,X'20'        TEST FOR DUPLICATE KEY ON ADD                
         BZ    ERROR               NO-LET GETMSG SORT IT OUT                    
         DC    H'0'                YES-DUMP TO INSURE PROPER RECOVERY           
*                                                                               
IOXX     MVI   IOFLAG,0                                                         
         B     EXXMOD                                                           
         SPACE 4                                                                
*--SET POSTING TYPE IN THE STATUS FIELD                                         
SETCTL   NTR1                                                                   
*--TEST FOR UPDATE COMMAND                                                      
         L     R3,AIOAREA          SET TO FILE AREA                             
         LA    R3,2(R3)       ADJUST SO KEY AND FILE AT THE SAME PLACE          
         CLI   IOWORK2,PUT                                                      
         BE    SETCTL20                                                         
         CLI   IOWORK2,ADDREC                                                   
         BE    SETCTL20                                                         
         LA    R3,KEY                                                           
         CLI   IOWORK2,WRITE                                                    
         BE    SETCTL20                                                         
         CLI   IOWORK2,ADD                                                      
         BNE   SETCTLEX                                                         
SETCTL20 CLI   NBPOSTYP,C'N'       NETWORK                                      
         BNE   *+12                                                             
         NI    20(R3),X'FC'                                                     
         B     SETCTLEX                                                         
         CLI   NBPOSTYP,C'C'       CABLE                                        
         BNE   *+16                                                             
         NI    20(R3),X'FC'                                                     
         OI    20(R3),X'01'                                                     
         B     SETCTLEX                                                         
         CLI   NBPOSTYP,C'S'       SYNDICATION                                  
         BNE   *+16                                                             
         NI    20(R3),X'FC'                                                     
         OI    20(R3),X'02'                                                     
         B     SETCTLEX                                                         
         CLI   NBPOSTYP,C'O'       OTHER                                        
         BNE   *+12                                                             
         NI    20(R3),X'FC'        OTHER                                        
         OI    20(R3),X'03'                                                     
SETCTLEX B     EXXMOD                                                           
         SPACE 4                                                                
*--SET LAST ACTIVITY BIT SETTINGS IN NEW LOG ELEMENT                            
*--R3 POINTS TO RECORD                                                          
SETSTAT  NTR1                                                                   
         USING NURECD,R3                                                        
*--CHECK IF WE ARE UPDATING A UNIT RECORD                                       
         CLC   IOFILE(3),=CL3'UNT'                                              
         BNE   SETSTEX                                                          
         CLC   IOCMND(6),=CL6'ADDREC'                                           
         BE    SETST040                                                         
         CLC   IOCMND(6),=CL6'PUTREC'                                           
         BNE   SETSTEX                                                          
SETST040 CLI   0(R3),X'04'                                                      
         BNE   SETSTEX                                                          
*                                                                               
SETST050 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
         USING NUSDRD,R4                                                        
*--CHECK IF ANY CHANGES WERE MADE                                               
         CLI   NUACTWHY,0                                                       
         BNE   SETST060                                                         
         CLI   NUACT2WY,0                                                       
         BE    SETSTEX                                                          
*                                                                               
SETST060 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'68',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    SETST070                                                         
         CLI   12(R1),X'06'        ELEMENT NOT FOUND                            
         BE    SETST200                                                         
         DC    H'0'                                                             
*                                                                               
SETST070 L     R2,12(R1)                                                        
         USING NUATV,R2                                                         
         XC    WORK(62),WORK                                                    
         MVC   WORK(2),0(R2)                                                    
         MVC   WORK+8(54),NUATVINF                                              
         MVC   0(62,R2),WORK                                                    
         MVC   NUATVINF(2),TODAYC                                               
         MVC   NUATVINF+2(1),NUACTWHY                                           
         MVC   NUATVINF+3(1),NUACT2WY                                           
         B     SETSTEX                                                          
         DROP  R2                                                               
*                                                                               
SETST200 XC    WORK(62),WORK                                                    
         LA    R2,WORK                                                          
         USING NUATV,R2                                                         
         MVC   NUATVEL(2),=XL2'683E'                                            
         MVC   NUATVINF(2),TODAYC                                               
         MVC   NUATVINF+2(1),NUACTWHY                                           
         MVC   NUATVINF+3(1),NUACT2WY                                           
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'68',(R3)),(R2),0                   
         B     SETSTEX                                                          
*                                                                               
*--FIRST TWO POSITIONS OF SPACES MUST BE RESET,BECUASE OF                       
*--OVERFLOW IN THE FIELD "WORK".                                                
SETSTEX  MVC   SPACES(2),=XL2'4040'                                             
         B     EXXMOD                                                           
         DROP  R2,R3,R4,RC                                                      
         EJECT                                                                  
* RETURN FROM MODULE AND ROUTINES                                               
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DC    XL16'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
         SPACE 2                                                                
* TABLE OF CORE-RESIDENT MODULE ADDRESSES                                       
*                                                                               
CORETAB  DS    0X                                                               
         DC    X'030E0F111415E0272817323312005D'                                
CORES    EQU   (*-CORETAB)                                                      
         SPACE 2                                                                
* TABLE OF BASE PROVIDED MODULES AND FACILITIES                                 
*                                                                               
BASETAB  DS    0F                                                               
         DC    A(0)                                                             
         DC    A(IO)                                                            
         DC    A(FVAL)                                                          
         DC    A(CHGBILL)                                                       
         DC    4A(0)                                                            
BASETABC EQU   (*-BASETAB)/L'BASETAB                                            
         SPACE 2                                                                
* TABLE OF I/O COMMANDS                                                         
*                                                                               
CMNDTAB  DS    0CL8                                                             
         DC    CL8'DMREAD'                                                      
         DC    CL8'DMRSEQ'                                                      
         DC    CL8'DMRDHI'                                                      
         DC    CL8'DMWRT'                                                       
         DC    CL8'DMADD'                                                       
         DC    CL8'GETREC'                                                      
         DC    CL8'PUTREC'                                                      
         DC    CL8'ADDREC'                                                      
         SPACE 2                                                                
* TABLE OF FILES/DIRECTORIES AND THEIR ATTRIBUTES                               
*                                                                               
FILTAB   DS    0XL17                                                            
         DC    CL7'SPTDIR',CL7'SPTFILE',AL1(0,13,14)                            
         DC    CL7'UNTDIR',CL7'UNTFIL',AL1(0,20,21)                             
         DC    CL7'STATION',CL7'STATION',AL1(DIR,17,0)                          
         DC    CL7'TRFDIR',CL7'TRFFILE',AL1(0,13,14)                            
         DC    CL7'CTFILE',CL7'CTFILE',AL1(DIR,25,0)                            
         DC    CL7'XSPDIR',CL7'XSPFIL',AL1(0,32,36)                             
         EJECT                                                                  
* TABLE OF ACTIONS AND CONTROL VALUES                                           
*                                                                               
ACTTAB   DS    0H                                                               
         DC    CL6'BP'             BUY PACKAGE                                  
         DC    X'02',AL1(BP),X'10',X'FD'                                        
         DC    AL1(FSTCURS)        INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(PASSEQ+LOKCHEK+STALOCK)                                      
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'BUY (ADD) A PACKAGE'                                        
*                                                                               
         DC    CL6'DP'             DISPLAY PACKAGE                              
         DC    X'02',AL1(DP),X'10',X'FD'                                        
         DC    AL1(READONLY)      INDICATORS                                    
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'DISPLAY PACKAGE'                                            
*                                                                               
         DC    CL6'CP'             CHANGE PACKAGE                               
         DC    X'02',AL1(CP),X'10',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'CHANGE PACKAGE'                                             
*                                                                               
         DC    CL6'DELP'           DELETE PACKAGE                               
         DC    X'04',AL1(DELP),X'10',X'FD'                                      
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'DELETE PACKAGE'                                             
*                                                                               
         DC    CL6'COPYP'          COPY PACKAGE                                 
         DC    X'05',AL1(COPYP),X'10',X'FD'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'COPY A PACKAGE'                                             
*                                                                               
         DC    CL6'B'              BUY UNIT                                     
         DC    X'01',AL1(B),X'12',X'FC'                                         
         DC    AL1(FSTCURS+SETPROGR)   INDICATORS                               
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' ' PROGRAM                                      
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'BUY A UNIT (MAKE-GOOD)'                                     
*                                                                               
         DC    CL6'BU'             BUY UNIT (UPLOAD)                            
         DC    X'01',AL1(B),X'12',X'FC'                                         
         DC    AL1(FSTCURS+SETPROGR)   INDICATORS                               
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' ' PROGRAM                                      
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'BUY A UNIT (MAKE-GOOD)'                                     
*                                                                               
         DC    CL6'D'              DISPLAY UNIT                                 
         DC    X'01',AL1(D),X'12',X'FC'                                         
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY SINGLE UNIT'                                        
*                                                                               
         DC    CL6'C'              CHANGE UNIT                                  
         DC    X'01',AL1(C),X'12',X'FC'                                         
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SINGLE UNIT'                                         
*                                                                               
         DC    CL6'CU'             CHANGE UNIT (UPLOAD)                         
         DC    X'01',AL1(C),X'12',X'FC'                                         
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SINGLE UNIT'                                         
*                                                                               
*        DC    CL6'CA'             CHANGE UNIT                                  
*        DC    X'02',AL1(C),X'12',X'FC'                                         
*        DC    AL1(SETPROGR)       INDICATORS                                   
*        DC    AL1(ISINGLE),XL2'00' CLIENT                                      
*        DC    AL1(ISINGLE),XL2'00'                                             
*        DC    AL1(ISINGLE),XL4'00'                                             
*        DC    AL1(ISINGLE),X'00'                                               
*        DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
*        DC    AL1(LOKCHEK)                                                     
*        DC    XL2'00'                                                          
*        DC    CL8'DATE'                                                        
*        DC    CL22'CHANGE UNIT ACCOUNTING'                                     
*                                                                               
         DC    CL6'DEL'            DELETE UNIT                                  
         DC    X'03',AL1(DEL),X'12',X'FC'                                       
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DELETE A UNIT'                                              
*                                                                               
         DC    CL6'DEA'            DISPLAY ESTIMATED ACTUAL                     
         DC    X'02',AL1(DEA),X'19',X'D0'                                       
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY ESTIMATED ACTUAL'                                   
*                                                                               
         DC    CL6'CEA'            CHANGE ESTIMATED ACTUAL                      
         DC    X'02',AL1(CEA),X'19',X'D0'                                       
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE ESTIMATED ACTUAL'                                    
*                                                                               
         DC    CL6'DD'             DEMO DISPLAY (VPH BASED)                     
         DC    X'02',AL1(DD),X'13',X'FB'                                        
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(VPHBASE)         VPH BASED PROGRAM                           
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY UNIT DEMOS'                                         
*                                                                               
         DC    CL6'DD'             DEMO DISPLAY (IMPRESSION BASED)              
         DC    X'02',AL1(DD),X'28',X'F1'                                        
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY UNIT DEMOS'                                         
*                                                                               
         DC    CL6'CD'             DEMO CHANGE (VPH BASED)                      
         DC    X'02',AL1(CD),X'13',X'FB'                                        
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(VPHBASE+LOKCHEK) VPH BASED PROGRAM                           
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE UNIT DEMOS'                                          
*                                                                               
         DC    CL6'CD'             DEMO CHANGE (IMPRESSION BASED)               
         DC    X'02',AL1(CD),X'28',X'F1'                                        
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE UNIT DEMOS'                                          
*                                                                               
         DC    CL6'CAL'            CALENDARIZE                                  
         DC    X'03',AL1(CAL),X'14',X'FA'                                       
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8'DATES'                                                       
         DC    CL22'CALENDARIZATION'                                            
*                                                                               
         DC    CL6'BM'             MULTIPLE BUY                                 
         DC    X'02',AL1(BM),X'14',X'FA'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8'DATES'                                                       
         DC    CL22'BUY MULTIPLE UNITS'                                         
*                                                                               
         DC    CL6'DM'             DISPLAY MULTIPLE                             
         DC    X'02',AL1(DM),X'15',X'00'                                        
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(VPHBASE)         VPH BASED PROGRAM                           
         DC    XL2'00'                                                          
         DC    CL8'KEYWORDS'                                                    
         DC    CL22'DISPLAY MULTIPLE UNITS'                                     
*                                                                               
         DC    CL6'DM'             DISPLAY MULTIPLE                             
         DC    X'02',AL1(DM),X'29',X'00'                                        
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'KEYWORDS'                                                    
         DC    CL22'DISPLAY MULTIPLE UNITS'                                     
*                                                                               
         DC    CL6'CM'             CHANGE MULTIPLE                              
         DC    X'02',AL1(CM),X'15',X'00'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(VPHBASE+LOKCHEK) VPH BASED PROGRAM                           
         DC    XL2'00'                                                          
         DC    CL8'KEYWORDS'                                                    
         DC    CL22'CHANGE MULTIPLE UNITS'                                      
*                                                                               
         DC    CL6'CM'             CHANGE MULTIPLE                              
         DC    X'02',AL1(CM),X'29',X'00'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'KEYWORDS'                                                    
         DC    CL22'CHANGE MULTIPLE UNITS'                                      
*                                                                               
         DC    CL6'COPYU'          COPY UNITS                                   
         DC    X'05',AL1(COPYU),X'16',X'F9'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'COPY UNITS'                                                 
*                                                                               
         DC    CL6'PACKU'          PACKAGE UPDATE                               
         DC    X'05',AL1(PACKU),X'17',X'F6'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PACKAGE UPDATE'                                             
*                                                                               
         DC    CL6'PD'             PUP TRANSFER DISPLAY                         
         DC    X'02',AL1(PD),X'18',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(PASSEQ+PUPTRNS+LOKCHEK+STALOCK)                              
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PUP TRANSFER DISPLAY'                                       
*                                                                               
         DC    CL6'PA'             PACKAGE ADD                                  
         DC    X'02',AL1(PA),X'18',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(PASSEQ+PUPTRNS+LOKCHEK+STALOCK)                              
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PUP TRANSFER ADD'                                           
*                                                                               
         DC    CL6'PC'             PACKAGE CHANGE                               
         DC    X'02',AL1(PC),X'18',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PUP TRANSFER CHANGE'                                        
*                                                                               
         DC    CL6'PT'             PUP TRANSFER                                 
         DC    X'02',AL1(PT),X'18',X'F3'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PUP TRANSFER'                                               
*                                                                               
         DC    CL6'IP'             INFO PACKAGE                                 
         DC    X'02',AL1(IP),X'20',X'F8'                                        
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE+IALL+IRANGE+DEFAULT+OPTIONAL),XL2'01FF'              
         DC    AL1(ISINGLE+IALL+DEFAULT+OPTIONAL),XL4'00'                       
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '                                               
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PACKAGE INFO'                                               
*                                                                               
         DC    CL6'IU'             INFO UNITS                                   
         DC    X'02',AL1(IU),X'21',X'F7'                                        
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE+IALL+IRANGE+DEFAULT+OPTIONAL),XL2'01FF'              
         DC    AL1(ISINGLE+IALL+DEFAULT+OPTIONAL),XL4'00'                       
         DC    AL1(ISINGLE+IALL),X'00'                                          
         DC    AL1(ISINGLE+IALL+IFILTER),CL6' '                                 
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'UNIT/PROGRAM INFO'                                          
*                                                                               
         DC    CL6'PROF'           DISPLAY UNIT PROFILES                        
         DC    X'04',AL1(PROFL),X'22',X'E3'                                     
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'PROFILE DISP FOR UNIT'                                      
*                                                                               
         DC    CL6'AUDIT'          AUDIT                                        
         DC    X'01',AL1(AUDIT),X'24',X'F4'                                     
         DC    AL1(READONLY)        INDICATORS                                  
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'AUDIT TRAIL FOR UNIT'                                       
*                                                                               
         DC    CL6'ASR'            CHANGE SPECIAL RATES                         
         DC    X'02',AL1(ASR),X'26',X'F5'                                       
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00' ESTIMATE                                    
         DC    AL1(ISINGLE),XL4'00' NETWORK                                     
         DC    AL1(ISINGLE),X'00'   PACKAGE                                     
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'ADD SPECIAL RATE'                                           
*                                                                               
         DC    CL6'CSR'            CHANGE SPECIAL RATES                         
         DC    X'02',AL1(CSR),X'26',X'F5'                                       
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00' ESTIMATE                                    
         DC    AL1(ISINGLE),XL4'00' NETWORK                                     
         DC    AL1(ISINGLE),X'00'   PACKAGE                                     
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SPECIAL RATE'                                        
*                                                                               
         DC    CL6'DSR'            DISPLAY SPECIAL RATES                        
         DC    X'02',AL1(DSR),X'26',X'F5'                                       
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00' ESTIMATE                                    
         DC    AL1(ISINGLE),XL4'00' NETWORK                                     
         DC    AL1(ISINGLE),X'00'   PACKAGE                                     
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY SPECIAL RATE'                                       
*                                                                               
         DC    CL6'TRANU'          TRANSFER UNITS                               
         DC    X'05',AL1(TRANU),X'27',X'F2'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'TRANSFER UNITS'                                             
*                                                                               
         DC    CL6'UPLOAD'         UPLOAD UNITS                                 
         DC    X'06',AL1(UPLOAD),X'40',X'00'                                    
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(NOEDIT),XL2'00' CLIENT                                       
         DC    AL1(NOEDIT),XL2'00'                                              
         DC    AL1(NOEDIT),XL4'00'                                              
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(STALOCK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'UPLOAD UNITS'                                               
*                                                                               
         DC    CL6'DPR'            ALLOCATE UNITS                               
         DC    X'03',AL1(DPR),X'42',X'E1'                                       
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'ALLOCATE UNITS'                                             
*                                                                               
         DC    CL6'CPR'            ALLOCATE UNITS                               
         DC    X'03',AL1(CPR),X'42',X'E1'                                       
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'ALLOCATE UNITS'                                             
*                                                                               
         DC    CL6'DH'             DISPLAY HISTORY                              
         DC    X'03',AL1(DH),X'43',X'E2'                                        
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'DISPLAY HISTORY'                                            
*                                                                               
         DC    CL6'STDRAF'         DRAFT ADD (STEWARD)                          
         DC    X'04',AL1(BM),X'44',X'FA'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8'DATES'                                                       
         DC    CL22'STEWARD DRAFT'                                              
*                                                                               
         DC    CL6'STBUY'          BUY UNIT (STEWARD)                           
         DC    X'04',AL1(B),X'45',X'FC'                                         
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' ' PROGRAM                                      
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'BUY A UNIT (MAKE-GOOD)'                                     
*                                                                               
         DC    CL6'STCHA'          CHANGE UNIT (STEWARD)                        
         DC    X'04',AL1(C),X'45',X'FC'                                         
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SINGLE UNIT'                                         
*                                                                               
         DC    CL6'STRCHA'         CHANGE UNIT (STEWARD)                        
         DC    X'04',AL1(REFC),X'45',X'FC'                                      
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SINGLE UNIT'                                         
*                                                                               
         DC    CL6'STRBUY'        CHANGE UNIT (STEWARD)                         
         DC    X'04',AL1(REFB),X'45',X'FC'                                      
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SINGLE UNIT'                                         
*                                                                               
         DC    CL6'STDEL'         DELETE UNIT (STEWARD)                         
         DC    X'04',AL1(DEL),X'45',X'FC'                                       
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DELETE A UNIT'                                              
*                                                                               
         DC    CL6'STFRBP'         BUY PACKAGE (STEWARD,FRONTRUNNER)            
         DC    X'02',AL1(BP),X'46',X'FD'                                        
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(PASSEQ+LOKCHEK+STALOCK)                                      
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'BUY (ADD) A PACKAGE'                                        
*                                                                               
         DC    CL6'STFRCP'         CHANGE PKG (STEWARD,FRONTRUNNER)             
         DC    X'02',AL1(CP),X'46',X'FD'                                        
         DC    AL1(0)               INDICATORS                                  
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'CHANGE A PACKAGE'                                           
*                                                                               
         DC    CL6'STFRDP'         DELETE PKG (STEWARD,FRONTRUNNER)             
         DC    X'02',AL1(DELP),X'46',X'FD'                                      
         DC    AL1(0)               INDICATORS                                  
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'DELETE A PACKAGE'                                           
*                                                                               
         DC    CL6'STFRBU'         BUY UNITS (FRONTRUNNER)                      
         DC    X'02',AL1(PT),X'47',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8'DATES'                                                       
         DC    CL22'FRONTRUNNER UNIT ADD'                                       
*                                                                               
         DC    CL6'STRCPY'         COPY UNITS                                   
         DC    X'05',AL1(COPYU),X'48',X'F9'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK+STALOCK)                                             
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'COPY UNITS'                                                 
*                                                                               
HELPENT  DC    CL6'HELP'           HELP                                         
         DC    X'02',AL1(HELP),X'00',X'FE'                                      
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0),XL2'00'      CLIENT                                       
         DC    AL1(0),XL2'00'                                                   
         DC    AL1(0),XL4'00'                                                   
         DC    AL1(0),X'00'                                                     
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'EXPLAINS ACTIONS'                                           
*                                                                               
ACTTABX  DC    X'FF'               END-OF TABLE                                 
         EJECT                                                                  
*                                                                               
* CAN'T CHANGE PREEMPTED UNITS                                                  
* NUUNITST IS THE STATUS ON THE UNIT TO BE CHANGED                              
* NBUNITST IS THE STATUS ON THE UNIT WHEN ORIGINALLY READ                       
*                                                                               
CHKPREM  NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'ST',BUYACT       ONLY FOR FALINK CHANGES                      
         JNE   CHKPREMX                                                         
         L     RF,AIOAREA                                                       
         USING NURECD,RF                                                        
         CLI   0(RF),X'04'         UNIT RECORD?                                 
         JNE   CHKPREMX                                                         
         TM    NUUNITST,X'40'      CURRENT PREEMPT STATUS                       
         JZ    CHKPREMX                                                         
         TM    NBUNITST,X'40'      PRIOR PREMPT STATUS                          
         JO    ERRPRMPT                                                         
CHKPREMX XIT1  REGS=(R0)                                                        
         DROP  RF                                                               
*                                                                               
ERRPRMPT LA    RF,BUYACTH                                                       
         ST    RF,FADDR                                                         
         MVI   FERN,PRMPTERR                                                    
         J     ERROR                                                            
         LTORG                                                                  
*                                                                               
* GETFLD - EXTRACT DATA FROM SCREEN FIELD                                       
*                                                                               
* ON ENTRY R2 POINTS TO FIELD HEADER                                            
* ON EXIT                                                                       
*        FADDR = A(FIELD HEADER)                                                
*        FLDH  CONTAINS FIELD HEADER                                            
*        FLD   CONTAINS EXTRACTED FIELD DATA SPACE FILLED                       
*        R0    CONTAINS BINARY VALUE OF DATA IF FIELD IS NUMERIC                
*                                                                               
GETFLDR  NTR1  BASE=*,LABEL=*                                                   
         ST    R2,FADDR                                                         
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         EX    R1,FLDMOVE                                                       
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLDR1 CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLDR2                                                         
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLDR1                                                      
         SPACE 1                                                                
GETFLDR2 STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GETFLDRX                                                         
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
         BZ    GETFLDRX                                                         
         CLI   FLDH+5,15           NO MORE THAN 15 DIGITS                       
         BNH   *+12                                                             
         NI    FLDH+4,X'FF'-X'08'  TURN OFF NUMERIC BIT                         
         B     GETFLDRX                                                         
         BCTR  R1,0                                                             
         EX    R1,FLDPACK                                                       
         CVB   R0,DUB                                                           
         SPACE 1                                                                
GETFLDRX XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
FLDMOVE  MVC   FLD(0),8(R2)                                                     
FLDPACK  PACK  DUB,FLD(0)                                                       
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*    UPDATE BILL KEYS WHEN UNIT RECORD KEY CHANGE/DELETE                        
*    ON INPUT  PARAM1 - C'D' (UNIT WAS DELETED SO DELETE BILLING)               
*                       A(UNIT KEY)                                             
*              PARAM2 - A(OLD UNIT KEY - ACTION CHANGE)                         
*              PARAM3 - A(AIOAREA FOR BILLING RECORDS)                          
*              PARAM4 - A(BILL KEY AREA FOR DATAMGR)                            
********************************************************************            
CHGBIL   NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)             A(NEW UNIT KEY)                             
         L     R5,8(R1)             A(AIOAREA FOR BILLING RECORDS)              
         L     R2,12(R1)            A(BILL KEY AREA FOR DATAMGR)                
*                                                                               
         CLI   0(R1),C'D'           UNIT WAS DELETED?                           
         BNE   CHGB100                                                          
         LA    R3,0(R3)             CLEAR HI-ORDER BIT                          
*                                                                               
         DC    H'00'                SHOULD NEVER BE ABLE TO DELETE              
         B     CHGBILX                                                          
*                                                                               
CHGB100  DS    0H                                                               
         L     R4,4(R1)             A(OLD UNIT KEY)                             
*                                                                               
         CLC   0(20,R3),0(R4)       WAS THERE A KEY CHANGE?                     
         BE    CHGBILX              NO - NO NEED TO UPDATE BILLING              
*                                                                               
         CLC   6(1,R3),6(R4)        WAS THE QUARTER HOUR CHANGED?               
         BE    CHGBILX                                                          
*                                                                               
         XC    0(100,R2),0(R2)      BUILD 0E06 KEY AND DELETE                   
         USING NUBRECD,R2                                                       
         USING NURECD,R4                                                        
*                                                                               
         MVC   0(2,R2),=X'0E06'                                                 
         MVC   NUBKAM,NUKAM         AGY/MEDIA                                   
         MVC   NUBKCLI,NUKCLT       CLIENT                                      
         MVC   NUBKNET,NUKNET       NETWORK                                     
         MVC   NUBKPROG,NUKPROG     PROGRAM                                     
         MVC   NUBKDATE,NUKDATE     AIR DATE                                    
         MVC   NUBKEST,NUKEST       ESTIMATE                                    
         MVC   NUBKSUB,NUKSUB       SUB-LINE                                    
         MVC   NUBKDPT,NUKDP        DAYPART                                     
         DROP  R2,R4                                                            
*                                                                               
         MVC   50(32,R2),0(R2)      SAVE AWAY BILLING KEY                       
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'XSPDIR  ',(R2),(R2)          
*                                                                               
CHGB125  DS    0H                                                               
         CLC   0(20,R2),50(R2)      FOUND A MATCHING BILLING RECORD?            
         BNE   CHGBILX                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL  ',36(R2),   *        
               (R5),DMWORK                                                      
*                                                                               
         CLC   0(2,R5),=X'0E06'     OLD BILLING KEY?                            
         BNE   CHGB130                                                          
         TM    33(R2),X'80'         X'0E06' MUST BE ACTIVE                      
         BZ    CHGBILX                                                          
         DC    H'00'                                                            
*                                                                               
CHGB130  CLC   0(2,R5),=X'0E0A'     NEW BILLING KEY?                            
         BNE   CHGBILX                                                          
*****    TM    33(R2),X'80'         X'0E06' HAS TO BE A PASSIVE                 
*****    BO    *+6                                                              
*****    DC    H'00'                                                            
*******************************************************************             
*        NEW X'0E0A' BILLING RECORD PROCESSING                                  
*******************************************************************             
CHGB150  DS    0H                                                               
         XC    0(100,R2),0(R2)                                                  
         MVC   0(32,R2),0(R5)       DELETE OLD 0E0A KEY                         
*                                                                               
         MVC   50(32,R2),0(R2)      SAVE AWAY BILLING KEY                       
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'XSPDIR  ',(R2),(R2)          
         CLC   0(21,R2),50(R2)      OLD 0E0A SHOULD BE THERE                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         OI    32(R2),X'80'         DELETE OLD 0E0A KEY                         
         GOTO1 VDATAMGR,DMCB,=C'DMWRT ',=C'XSPDIR  ',(R2),(R2),0                
*                                                                               
         OI    34(R5),X'80'         DELETE OLD 0E0A RECORD                      
         GOTO1 VDATAMGR,DMCB,=C'PUTREC',=C'XSPFIL  ',36(R2),(R5),DMWORK         
*******************************************************************             
* CHECK IF NEW 0E0A EXISTS, IF SO IT MUST BE DELETED SO RESTORE                 
*******************************************************************             
         XC    0(100,R2),0(R2)      CHECK IF NEW 0E0A ALREADY EXISTS            
         USING NUBRECD,R2                                                       
         USING NURECD,R3                                                        
*                                                                               
         MVC   0(2,R2),=X'0E0A'                                                 
         MVC   NUBK0AM,NUKAM        AGY/MEDIA                                   
         MVC   NUBK0CLI,NUKCLT      CLIENT                                      
         MVC   NUBK0DAT,NUKDATE     AIR DATE                                    
         MVC   NUBK0TIM,NUKTIME     START 1/4 HOUR                              
         MVC   NUBK0NET,NUKNET      NETWORK                                     
         MVC   NUBK0PRG,NUKPROG     PROGRAM                                     
         MVC   NUBK0EST,NUKEST      ESTIMATE                                    
         MVC   NUBK0SUB,NUKSUB      SUB-LINE                                    
         MVC   NUBK0DPT,NUKDP       DAYPART                                     
         MVC   NUBK0BDT,21(R5)      BILLING RUN DATE                            
         DROP  R2,R3                                                            
*                                                                               
         MVC   50(32,R2),0(R2)      SAVE AWAY BILLING KEY                       
         GOTO1 VDATAMGR,DMCB,(X'88',=C'DMRDHI'),=C'XSPDIR  ',(R2),(R2)          
         CLC   0(21,R2),50(R2)      DOES NEW 0E0A ALREADY EXIST?                
         BNE   CHGB175                                                          
*                                                                               
         TM    32(R2),X'80'         THEN IT MUST ALREADY BE DELETED             
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         NI    32(R2),X'FF'-X'80'   UNMARK IT DELETED                           
         GOTO1 VDATAMGR,DMCB,=C'DMWRT ',=C'XSPDIR  ',(R2),(R2),0                
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL  ',36(R2),   *        
               (R5),DMWORK                                                      
         NI    34(R5),X'FF'-X'80'    UNMARK 0E0A RECORD                         
         GOTO1 VDATAMGR,DMCB,=C'PUTREC',=C'XSPFIL  ',36(R2),(R5),DMWORK         
*                                                                               
         MVC   FULL,36(R2)          SAVE AWAY DISK ADDRESS                      
         B     CHGB190                                                          
*******************************************************************             
* NEW 0E0A DOES NOT EXIST, SO ADD NEW 0E0A BILLING RECORD                       
*******************************************************************             
CHGB175  DS    0H                                                               
         USING NURECD,R3            MOVE IN NEW 0E0A KEY INTO RECORD            
         USING NUBRECD,R5           AND DO ADDREC                               
*                                                                               
         NI    34(R5),X'FF'-X'80'   MARK UNDELETED                              
*                                                                               
         MVC   0(2,R5),=X'0E0A'                                                 
         MVC   NUBK0AM,NUKAM        AGY/MEDIA                                   
         MVC   NUBK0CLI,NUKCLT      CLIENT                                      
         MVC   NUBK0DAT,NUKDATE     AIR DATE                                    
         MVC   NUBK0TIM,NUKTIME     START 1/4 HOUR                              
         MVC   NUBK0NET,NUKNET      NETWORK                                     
         MVC   NUBK0PRG,NUKPROG     PROGRAM                                     
         MVC   NUBK0EST,NUKEST      ESTIMATE                                    
         MVC   NUBK0SUB,NUKSUB      SUB-LINE                                    
         MVC   NUBK0DPT,NUKDP       DAYPART                                     
         DROP  R3,R5                                                            
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 VDATAMGR,DMCB,=C'ADDREC',=C'XSPFIL  ',36(R2),(R5),DMWORK         
         L     RF,DMCB+8                                                        
         MVC   FULL,0(RF)           SAVE AWAY NEW DISK ADDRESS                  
*                                                                               
CHGB190  DS    0H                                                               
         XC    0(100,R2),0(R2)      BUILD NEW 0E06 PASSIVE KEY                  
         USING NUBRECD,R2                                                       
         USING NURECD,R3                                                        
*                                                                               
         MVC   0(2,R2),=X'0E06'                                                 
         MVC   NUBKAM,NUKAM         AGY/MEDIA                                   
         MVC   NUBKCLI,NUKCLT       CLIENT                                      
         MVC   NUBKNET,NUKNET       NETWORK                                     
         MVC   NUBKPROG,NUKPROG     PROGRAM                                     
         MVC   NUBKDATE,NUKDATE     AIR DATE                                    
         MVC   NUBKEST,NUKEST       ESTIMATE                                    
         MVC   NUBKSUB,NUKSUB       SUB-LINE                                    
         MVC   NUBKDPT,NUKDP        DAYPART                                     
         MVC   NUBKBDAT,21(R5)      BILLING RUN DATE                            
*                                                                               
         MVC   50(32,R2),0(R2)      SAVE AWAY BILLING KEY                       
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'XSPDIR  ',(R2),(R2)          
         CLC   0(20,R2),50(R2)      THIS PASSIVE SHOULD ALREADY EXIST           
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   NUBDA,FULL           DISK ADDRESS OF NEW 0E0A RECORD             
         DROP  R2,R3                                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMWRT ',=C'XSPDIR  ',(R2),(R2),0                
*                                                                               
         MVC   50(32,R2),0(R2)      SAVE AWAY BILLING KEY                       
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMRSEQ'),=C'XSPDIR  ',(R2),(R2)          
         B     CHGB125                                                          
*                                                                               
CHGBILX  DS    0H                                                               
         J     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* FIND THE SCREEN LENGTH UPDATE HEADER+5 TO REFLECT THE INPUT                   
* R2 = SCREEN HEADER OF FIELD                                                   
*=========================================================                      
FINDLEN  NTR1  BASE=*,LABEL=*                                                   
         ZIC   RE,5(R2)             MAXUMUM INPUT                               
         SR    RF,RF                ACTUAL INPUT                                
         LA    R3,8(R2)             SCREEN FIELD                                
*                                                                               
FNDLN40  CLI   0(R3),X'40'                                                      
         BNH   FNDLN80                                                          
         LA    R3,1(R3)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,FNDLN40                                                       
*                                                                               
FNDLN80  STCM  RF,1,5(R2)           RESET THE LENGTH                            
         J     EXXMOD                                                           
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
CHKSECU  NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)             A(CLIENT RECORD)                            
         USING CLTHDRD,R3                                                       
         L     R4,4(R1)             A(CLIENT 3 BYTE NAME)                       
         L     R5,ATIA              R5 POINTS TO BUYVALS                        
         USING BUYVALD,R5                                                       
*                                                                               
*  CHECK SECURITY AGAINST NEW OFFICER CALL                                      
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
*                                                                               
         L     RE,ATWA                                                          
         A     RE,=A(BASETWA)                                                   
         USING SVAREA,RE                                                        
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,0(R4)                                                     
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,AGYMED                                                  
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         LA    RF,SVSECRET                                                      
         ST    RF,OFCSECD                                                       
         DROP  R1,RE                                                            
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         BE    CALLOFEX                                                         
*                                                                               
         MVI   FERN,SCTYERR                                                     
         J     ERROR                                                            
*                                                                               
CALLOFEX J     EXXMOD                                                           
         LTORG                                                                  
         DROP  R3,R5                                                            
         EJECT                                                                  
*=========================================================                      
* CALL DEMOCON DT DECODE OR ENCODE THREE BYTE DEMO STRING                       
*                                                                               
* R1 = 1 DO A ENCODE                                                            
* R1 = 2 DO A DECODE                                                            
*                                                                               
* ENCDEMO - ENCODED DEMO VALUE                                                  
* DECDEMO - DECODED DEMO VALUE                                                  
*=========================================================                      
ENDEDEMC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,DBLOCKA                                                       
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         CLI   0(R1),1           ARE WE ENCODING                                
         BNE   ENDEC50                                                          
* ENCODE THE DEMO                                                               
         XC    ENCDEMO,ENCDEMO                                                  
         GOTO1 VDEMOCON,DMCB,(1,DECDEMO),('DEMOCON_17',ENCDEMO),       X        
               DBLOCK,DECDEMO+3                                                 
         B     ENDECEX                                                          
*                                                                               
* DECODE THE DEMO                                                               
ENDEC50  CLI   0(R1),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DECDEMO,DECDEMO                                                  
         GOTO1 VDEMOCON,DMCB,(1,ENCDEMO),('DEMOCON_16',DECDEMO),       X        
               DBLOCK,DECDEMO+3                                                 
         B     ENDECEX                                                          
*                                                                               
ENDECEX  J     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* SET UP THE ACTION FIELD FOR STEWARD REQUESTS                                  
*                                                                               
SETACT   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA4                                                      
         USING BUYDRFTD,R3                                                      
         MVI   BUYACTH,72                                                       
         MVC   BUYACT,SPACES                                                    
         LA    R2,BUYACT                                                        
*                                                                               
         LA    RE,STACTTAB                                                      
*                                                                               
SETACT10 CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RDRTYPE,0(RE)                                                    
         BE    SETACT20                                                         
         LA    RE,11(RE)                                                        
         B     SETACT10                                                         
*                                                                               
SETACT20 MVC   0(8,R2),2(RE)        MOVE IN ACTION                              
         ZIC   R1,10(RE)                                                        
         AR    R2,R1                                                            
*                                                                               
         CLI   RDRBDATE,X'40'                                                   
         BNH   SETACT80                                                         
         MVI   0(R2),C','                                                       
         GOTO1 VDATCON,DMCB,(4,RDRBDATE),(4,1(R2))                              
         LA    R2,5(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   RDRSLINE,X'40'                                                   
         BNH   SETACT30                                                         
         MVI   0(R2),C'-'                                                       
         MVC   1(3,R2),RDRSLINE                                                 
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
SETACT30 CLC   RDRTYPE,=CL2'CH'                                                 
         BE    SETACT35                                                         
         CLC   RDRTYPE,=CL2'RC'                                                 
         BNE   SETACT40                                                         
SETACT35 TM    RUPSTAT-RUPSEQ(R3),X'80'    CHECK FOR EST=Y OPTION               
         BZ    SETACT40                                                         
         MVC   0(6,R2),=CL6',EST=Y'                                             
         LA    R2,6(R2)                                                         
SETACT40 CLC   RDRTYPE,=CL2'DR'     IF MODE NOT DRAFT                           
         BNE   SETACT80             GO STRAIGHT TO REASON CHECK                 
         CLI   RDRWEEKS,X'40'                                                   
         BNH   SETACT50                                                         
         MVI   0(R2),C'-'                                                       
         MVC   1(2,R2),RDRWEEKS                                                 
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'W'                                                       
         LA    R2,1(R2)                                                         
SETACT50 CLI   RDRPWEEK,X'40'                                                   
         BNH   SETACT80                                                         
         MVI   0(R2),C'*'                                                       
         MVC   1(3,R2),RDRPWEEK                                                 
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
SETACT80 CLI   RDRREAS,X'40'                                                    
         BNH   SETACTEX                                                         
         MVC   0(4,R2),=CL4',RS='                                               
         MVC   4(3,R2),RDRREAS                                                  
SETACTEX J     EXXMOD                                                           
*                                                                               
STACTTAB DC    CL2'PB',CL8'STFRBP',XL1'06'   BUY PACKAGE                        
         DC    CL2'PC',CL8'STFRCP',XL1'06'   CHANGE PACKAGE                     
         DC    CL2'PD',CL8'STFRDP',XL1'06'   DELETE PACKAGE                     
         DC    CL2'FB',CL8'STFRBU',XL1'06'                                      
         DC    CL2'DR',CL8'STDRAF',XL1'06'                                      
         DC    CL2'CH',CL8'STCHA',XL1'05'                                       
         DC    CL2'BU',CL8'STBUY',XL1'05'                                       
         DC    CL2'DE',CL8'STDEL',XL1'05'                                       
         DC    CL2'RB',CL8'STRBUY',XL1'06'                                      
         DC    CL2'RC',CL8'STRCHA',XL1'06'                                      
         DC    CL2'CU',CL8'STRCPY',XL1'06'   COPYU                              
         DC    XL1'FF'                                                          
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* HELP SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYFED                                                       
         ORG   TWAD+BASETWA                                                     
SVAREA   DS    0H                                                               
SVPROF1  DS    CL16                BUY PROFILE 1 (BUYPROF)                      
SVPROF2  DS    CL16                BUY PROFILE 2 (BUYPROF2)                     
SVPROF3  DS    CL16                BUY PROFILE 3 (NETPROF)                      
SVINDS9  DS    XL1                 NBINDS9 INDICATOR                            
*                                                                               
LSTAGY   DS    CL2                 LAST AGENCY                                  
LSTCLI   DS    CL3                 LAST CLIENT                                  
LSTOFF   DS    CL1                 LAST OFFICE                                  
         SPACE 2                                                                
LINELEN  EQU   HELLIN2H-HELLIN1H                                                
LINES    EQU   (HELLAST-HELLIN1H)/LINELEN                                       
*                                                                               
MAXNDEMS EQU   50                                                               
*                                                                               
SVSECRET DS    CL1024              SAVED SECRET BLOCK                           
         EJECT                                                                  
* DSECT TO COVER I/O ROUTINE LOCAL STORAGE                                      
*                                                                               
IOWORKD  DSECT                                                                  
IOWORK   DS    CL96                FOR GETREC, PUTREC, AND ADDREC               
IOPARM   DS    6F                  PARAMETER LIST                               
IOWORK1  DS    X                                                                
IOWORK2  DS    X                                                                
IOWORK3  DS    X                                                                
IOCMND   DS    CL7                 DATA MANAGER COMMAND                         
IODIR    DS    CL7                 DIRECTORY NAME                               
IOFILE   DS    CL7                 FILE NAME                                    
IOVALS   DS    0XL3                                                             
IOEXCPT  DS    X                   EXCEPTION IOFLAG VALUES FOR FILE             
IOKEYL   DS    X                   KEY LENGTH                                   
IODADSP  DS    X                   DISPLACEMENT TO DISK ADDRESS                 
IOWORKX  EQU   *                                                                
         EJECT                                                                  
* COMFACSD                                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
         SPACE 2                                                                
       ++INCLUDE NAVDSECTS                                                      
         SPACE 2                                                                
* SPGENAGY (AGYHDRD)                                                            
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         SPACE 2                                                                
* SPGENCLT (CLTHDRD)                                                            
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         SPACE 2                                                                
* SPGENEST (ESTHDRD)                                                            
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         SPACE 2                                                                
       ++INCLUDE NEGENUBILL                                                     
         SPACE 2                                                                
NADHDRD  DSECT                                                                  
       ++INCLUDE SPGENNAD                                                       
         SPACE 2                                                                
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         SPACE 2                                                                
DPTRECD  DSECT                                                                  
       ++INCLUDE NEGENDPT                                                       
         SPACE 2                                                                
* DDCOREQUS                                                                     
       ++INCLUDE DDCOREQUS                                                      
         SPACE 2                                                                
* DDOFFICED                                                                     
       ++INCLUDE DDOFFICED                                                      
         SPACE 2                                                                
* DDWSSVR                                                                       
       ++INCLUDE FAWSSVRD                                                       
         SPACE 2                                                                
* FASECRETD                                                                     
       ++INCLUDE FASECRETD                                                      
         SPACE 2                                                                
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         SPACE 2                                                                
* FALOCKETD                                                                     
       ++INCLUDE FALOCKETD                                                      
         SPACE 2                                                                
       ++INCLUDE DDGLVXCTLD                                                     
         SPACE 2                                                                
       ++INCLUDE FAUTL                                                          
       ++INCLUDE NETBLKXTND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEBUY00   02/26/20'                                      
         END                                                                    
