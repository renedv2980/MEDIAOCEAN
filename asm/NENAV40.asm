*          DATA SET NENAV40    AT LEVEL 003 AS OF 05/02/16                      
*PHASE T31840A                                                                  
NENAV40  TITLE 'T31840 - Optica Network Traffic Instructions'                   
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=NETSYSQ,              +        
               LOADFACSOFF=Y,SERVERTYPE=TSTSTEW,IDF=Y,WORKERKEY=NEQN,  +        
               SYSPHASE=SYSPHASE,SEGMENT=Y,APPEND=Y,                   +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED)                             
                                                                                
CODE     NMOD1 0,**NN40**                                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
                                                                                
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK control block)                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JNZ   INIT02                                                           
         L     R9,LP_ABLK1         Online - root provides WORKD/SAVED           
         L     R8,LP_ABLK2                                                      
         J     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(global w/s)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R8,WORKD+(((WORKLOFF+7)/8)*8)                                    
         USING SAVED,R8            R8=A(save w/s)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK CALLING MODE                  
         MVC   ACOMFACS,RCOMFACS                                                
         ST    R5,ALP              Save A(DDLINK parameter list)                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         J     RUNSTR                                                           
EXITY    CR    RB,RB                                                            
         J     EXIT                                                             
EXITN    LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         DROP  R6,R7,RB                                                         
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    Test first for run                           
         JNE   PRCWRK                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    RUNSTR02            No                                           
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    Set A(index routines 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    Set A(index routines 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
                                                                                
RUNSTR02 DS    0H                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VWSSVR,CWSSVR                                                    
         J     EXITY                                                            
         DROP  RF                                                               
                                                                                
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    Test 'process work' mode                     
         JNE   RUNREQ                                                           
         LA    R0,QVALUES                                                       
         LHI   R1,QVALUEX-QVALUES                                               
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   EXITY                                                            
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         J     EXITY                                                            
         EJECT                                                                  
*====================================================================           
* THIS TRANSFERS CONTROL TO SPTRA1F TO BUILD LIST OF PROGRAMS                   
*====================================================================           
                                                                                
* THIS GETS THE REQUEST FIELDS FROM OPTICA                                      
                                                                                
QN00     LKREQ H,M#BLDNI,QN00IN,NEXTREQ=QN10                                    
CLTCD    LKREQ F,001,(D,B#WORKD,QCLTA),CHAR,TEXT=(*,CLILIT)                     
NETWK    LKREQ F,002,(D,B#WORKD,QNETA),VSTR,TEXT=(*,NETLIT)                     
****NETWK    LKREQ F,002,(D,B#WORKD,QNETA),CHAR,TEXT=(*,NETLIT)                 
PROGCD   LKREQ F,003,(D,B#WORKD,QPRG),CHAR,TEXT=(*,PROGLIT)                     
PERIOD   LKREQ F,004,(D,B#WORKD,QPER),CHAR,TEXT=(*,PERLIT)                      
OPTFAX   LKREQ F,005,(D,B#WORKD,QOPTFAX),CHAR,TEXT=(*,FAXLIT)                   
OPTIONS  LKREQ F,006,(D,B#WORKD,QOPTIONS),CHAR,TEXT=(*,OPTLIT)                  
         LKREQ E                                                                
*                                                                               
CLILIT   DC    C'Client'                                                        
NETLIT   DC    C'Network'                                                       
PROGLIT  DC    C'Program'                                                       
PERLIT   DC    C'Period'                                                        
FAXLIT   DC    C'Fax?'                                                          
OPTLIT   DC    C'Options'                                                       
UNIQLIT  DC    C'Unique ID'                                                     
ALLLIT   DC    C'Run all?'                                                      
*                                                                               
* THIS DUMMY ROUTINE CALLS AUTOGLOB AND EXITS TO DO A XFRCTL                    
*                                                                               
QN00IN   LKOUT H                                                                
*                                                                               
         LKOUT R,X'002A'           UNIQUE REPLY NUMBER                          
         LKOUT P,,QN00OUT                                                       
         LKOUT E                                                                
         LKOUT X                                                                
*                                                                               
QN00OUT  DS   0H                                                                
         XC    SVNEXT,SVNEXT                                                    
         MVI   BYTE,C'B'           SET MODE=BLD                                 
         BRAS  RE,QNGLOB                                                        
         J     EXITN                                                            
         EJECT                                                                  
*==================================================================             
* THIS ROUTINE GETS CONTROL ON RETURN FROM SPTRA1F                              
* THERE ISN'T REALLY ANY INPUT, SO GET SOMETHING ... ANYTHING!                  
*==================================================================             
                                                                                
QN10     LKREQ H,M#DOWNNI,QN10IN,NEXTREQ=QN20                                   
         LKREQ F,001,(D,B#WORKD,QMEDA),CHAR,TEXT=(*,DUMMYLIT)                   
         LKREQ E                                                                
*                                                                               
DUMMYLIT DC    C'DUMMY DATA'                                                    
                                                                                
*===================================================================            
* THIS ROUTINE SENDS THE LIST OF PROGRAMS BUILT IN WSSVR TO OPTICA              
*===================================================================            
                                                                                
QN10IN   LKOUT H                                                                
                                                                                
         LKOUT R,X'002A'           UNIQUE REPLY NUMBER                          
         LKOUT C,1,(A,QN10ARY)                                                  
         LKOUT C,98,(D,B#WORKD,QERRCODE),CHAR,ND=Y                              
         LKOUT C,99,(D,B#WORKD,QERRTEXT),CHAR,ND=Y                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
QN10ARY  LKOUT A,(R,QN10SND),MULTIROW=Y                                         
         LKOUT C,001,(D,B#WORKD,NIPROG),CHAR                                    
         LKOUT C,002,(D,B#WORKD,NICURREV),CHAR                                  
         LKOUT C,003,(D,B#WORKD,NIPRVREV),CHAR                                  
         LKOUT C,004,(D,B#WORKD,NIUNITS),CHAR                                   
         LKOUT C,005,(D,B#WORKD,NIFEEDS),CHAR                                   
         LKOUT C,006,(D,B#WORKD,NINOCML),CHAR                                   
         LKOUT C,007,(D,B#WORKD,NIUNAL),CHAR                                    
         LKOUT C,008,(D,B#WORKD,NIDEL),CHAR                                     
         LKOUT E                                                                
         EJECT                                                                  
*=================================================================              
* SEND THE PROGRAMS FROM WSSVR BUFFER                                           
*=================================================================              
                                                                                
QN10SND  BRAS  RE,QNERR            CHECK FOR ERRORS                             
         JNE   NOMORE                                                           
*                                                                               
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
**NOP**  MVC   NAVINP(3),=C'NE0'   TRY FOR EXPANDED OUTPUT                      
         DROP  RE                                                               
*                                                                               
         ICM   R4,15,SVNEXT        TEST FIRST TIME                              
         JNZ   Q10SN02             NO                                           
*                                                                               
         XC    WORK2(100),WORK2                                                 
         LA    R1,WORK2                                                         
         USING FAWSSVRD,R1                                                      
*                                                                               
         MVC   FAWSTOKN,=C'TR1F'   TOKEN                                        
         OI    FAWSACTN,FAWSURST                                                
         MVC   FAWSADR,AIO3                                                     
         MVC   FAWSLEN,=H'8192'                                                 
         GOTOR VWSSVR,(R1)                                                      
*                                                                               
         CLI   FAWSRTN,FAWSROK                                                  
         JNE   *+2                                                              
         DROP  R1                                                               
*                                                                               
         L     R4,AIO3                                                          
         USING PRGTABLED,R4                                                     
*                                                                               
         OC    PRGPRG,PRGPRG                                                    
         JZ    Q10SNERR                                                         
         J     Q10SN04                                                          
*                                                                               
Q10SN02  OC    PRGPRG,PRGPRG                                                    
         JZ    NOMORE                                                           
*                                                                               
Q10SN04  MVC   NIPROG(6),PRGPRG                                                 
*                                                                               
         OC    PRGCREVD,PRGCREVD   CURRENT REVISION                             
         JZ    Q10SN08                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(2,PRGCREVD),(8,NICURREV)                           
         LLC   R0,PRGCREVN         REV NUM                                      
         MVI   NICURREV+8,C'-'                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NICURREV+9(2),DUB                                                
*                                                                               
         MVC   NIPRVREV(8),=C'ORIGINAL'                                         
         OC    PRGPREVD,PRGPREVD   PREVIOUS REVISION                            
         JZ    Q10SN08                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(2,PRGPREVD),(8,NIPRVREV)                           
         MVI   NIPRVREV+8,C'-'                                                  
         LLC   R0,PRGPREVN         REV NUM                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NIPRVREV+9(2),DUB                                                
*                                                                               
Q10SN08 SR     R0,R0                                                            
         ICM   R0,3,PRGUNITS                                                    
         LA    R1,NIUNITS                                                       
         BRAS  RE,CVD                                                           
*                                                                               
         ICM   R0,3,PRGFEEDS                                                    
         LA    R1,NIFEEDS                                                       
         BRAS  RE,CVD                                                           
*                                                                               
         ICM   R0,3,PRGUNAS                                                     
         LA    R1,NINOCML                                                       
         BRAS  RE,CVD                                                           
*                                                                               
         ICM   R0,3,PRGUNAL                                                     
         LA    R1,NIUNAL                                                        
         BRAS  RE,CVD                                                           
*                                                                               
         ICM   R0,3,PRGDEL                                                      
         LA    R1,NIDEL                                                         
         BRAS  RE,CVD                                                           
*                                                                               
         LA    R4,PRGNEXT                                                       
         ST    R4,SVNEXT                                                        
*                                                                               
Q10SNX   MVC   LP_ADATA,LP_ABLK1                                                
         J     EXITY                                                            
*                                                                               
CVD      CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
         BR    RE                                                               
*                                                                               
Q10SNERR MVC   QERRCODE,=C'9999'                                                
         MVC   QERRTEXT(25),=C'NO INSTRUCTIONS TO BE RUN'                       
         J     NOMORE                                                           
*                                                                               
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
         J     EXITY                                                            
         EJECT                                                                  
*====================================================================           
* THIS ROUTINE MOVES THE PROGRAM LIST TO THE WSSVR BUFFER IN THE UTL            
* THEN TRANSFERS CONTROL TO SPTRA1F TO SUBMIT A SOON JOB FOR                    
* SELECTED PROGRAMS OR ALL                                                      
*====================================================================           
                                                                                
* THIS GETS THE REQUEST FIELDS FROM OPTICA                                      
                                                                                
QN20     LKREQ H,M#SUBNI,QN20IN,NEXTREQ=QN30                                    
CLTCD    LKREQ F,001,(D,B#WORKD,QCLTA),CHAR,TEXT=(*,CLILIT)                     
NETWK    LKREQ F,002,(D,B#WORKD,QNETA),VSTR,TEXT=(*,NETLIT)                     
*****NETWK    LKREQ F,002,(D,B#WORKD,QNETA),CHAR,TEXT=(*,NETLIT)                
PROGCD   LKREQ F,003,(D,B#WORKD,QPRG),CHAR,TEXT=(*,PROGLIT)                     
PERIOD   LKREQ F,004,(D,B#WORKD,QPER),CHAR,TEXT=(*,PERLIT)                      
OPTFAX   LKREQ F,005,(D,B#WORKD,QOPTFAX),CHAR,TEXT=(*,FAXLIT)                   
OPTIONS  LKREQ F,006,(D,B#WORKD,QOPTIONS),CHAR,TEXT=(*,OPTLIT)                  
UNIQID   LKREQ F,007,(D,B#WORKD,QUNIQID),CHAR,TEXT=(*,UNIQLIT)                  
OPTALL   LKREQ F,008,(D,B#WORKD,QOPTALL),CHAR,TEXT=(*,ALLLIT)                   
PROGS    LKREQ F,014,(I,B#WORKD,PRGNUMI),CHAR,TEXT=(*,PRGTXT),OLEN=6,  X        
               LIST=F                                                           
         LKREQ E                                                                
*                                                                               
PRGTXT   DC    C'Programs'                                                      
                                                                                
QN20IN   LKOUT H                                                                
         LKOUT R,X'002A'           UNIQUE REPLY NUMBER                          
         LKOUT P,,QN20OUT                                                       
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*==============================================================                 
* THIS DUMMY ROUTINE CALLS QNGLOB AND EXITS TO DO A XFRCTL                      
*==============================================================                 
                                                                                
QN20OUT  DS    0H                                                               
         MVI   BYTE,C'S'           SET MODE=SUB                                 
         BRAS  RE,QNGLOB                                                        
                                                                                
* NOW MOVE PROGRAM DATA TO WSSVR BUFFER UNLESS ALL PROGRAM REQUEST              
                                                                                
         MVC   QERRCODE,=C'0000'                                                
         XC    QERRTEXT,QERRTEXT                                                
*                                                                               
         CLI   QRUNALL,C'Y'           IF RUN=ALL                                
         JE    QN24                                                             
*                                                                               
         XC    WORK2(100),WORK2                                                 
         LA    R1,WORK2                                                         
         USING FAWSSVRD,R1                                                      
*                                                                               
         L     R4,AIO3                                                          
*                                                                               
         ICM   RE,7,APRGLST        GET ARRAY ADDRESS                            
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN-LW_D(RE) NUMBER OF ENTRIES                          
         JZ    QNRUNERR                                                         
         AHI   RE,LW_LN2Q                                                       
*                                                                               
QN22     MVC   0(6,R4),0(RE)       MOVE FIRST ENTRY                             
         LA    R4,6(R4)                                                         
         LA    RE,6(RE)                                                         
         JCT   RF,QN22                                                          
*                                                                               
         MVC   FAWSTOKN,=C'TR1F'   TOKEN                                        
         OI    FAWSACTN,FAWSUSVE                                                
         MVC   FAWSADR,AIO3                                                     
         MVC   FAWSLEN,=H'8192'                                                 
         GOTOR VWSSVR,(R1)                                                      
*                                                                               
         CLI   FAWSRTN,FAWSROK                                                  
         JNE   *+2                                                              
*                                                                               
QN24     J     EXITN                                                            
*                                                                               
QN30     LKREQ H,M#ENDNI,QN30IN,NEXTREQ=REQUESTX                                
         LKREQ F,001,(D,B#WORKD,QMEDA),CHAR,TEXT=(*,DUMMYLIT)                   
         LKREQ E                                                                
*                                                                               
QN30IN   LKOUT H                   DUMMY OUTPUT FOR GLOBBER CALL                
                                                                                
         LKOUT R,X'002D'           UNIQUE REPLY NUMBER                          
         LKOUT P,,QN30OUT                                                       
         LKOUT C,98,(D,B#WORKD,QERRCODE),CHAR,ND=Y                              
         LKOUT C,99,(D,B#WORKD,QERRTEXT),CHAR,ND=Y                              
         LKOUT E                                                                
         LKOUT X                                                                
*                                                                               
QN30OUT  BRAS  RE,QNERR            CHECK FOR ERRORS ON RETURN                   
         JNE   NOMORE                                                           
         MVC   QERRCODE,=C'0000'                                                
         MVC   QERRTEXT(18),=C'SOON JOB SUBMITTED'                              
         J     EXITY                                                            
*                                                                               
QNRUNERR MVC   QERRCODE,=C'9999'                                                
         MVC   QERRTEXT(25),=C'NO INSTRUCTIONS TO BE RUN'                       
         J     EXITY                                                            
*                                                                               
QNERR    NTR1                                                                   
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,69,GLVBUY1                           
         CLI   8(R1),0                                                          
         JNE   EXITY               IF NO ELEM, CLEAN RETURN                     
         CLI   ELEM,C'<'           ERRORS START WITH THIS                       
         JNE   EXITY                                                            
         MVC   QERRCODE,ELEM+1                                                  
         MVC   QERRTEXT,ELEM+6                                                  
         GOTO1 VGLOBBER,DMCB,=C'CLEAR'                                          
         J     EXITN                                                            
*                                                                               
REQUESTX LKREQ X                                                                
         EJECT                                                                  
*============================================================                   
                                                                                
* SUMMARY OF GLOBALS USED                                                       
* GLVXREC                                                                       
* GLVXACT                                                                       
* GLVSPRNT = SOON,XXX                                                           
* GLVSPCLT                                                                      
* GLVSPSTA                                                                      
* GLVSPPER                                                                      
* GLVSPTRF   COVERED BY DDGLVSPTRF                                              
* GLVBUY1  = OPTIONS FIELD (60) ON INPUT, ERROR FIELD ON OUTPUT                 
*            IF FIRST CHARACTER IS <                                            
* GLVBUY2  = UNIQUE ID FOR PDF                                                  
*============================================================                   
                                                                                
QNGLOB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'CLEAR'  CLEAR ALL GLOBAL VALUES                 
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(4),=C'NINS'                                                 
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,4,GLVXREC                            
*                                                                               
         MVC   ELEM(4),=C'GEN '                                                 
         GOTO1 (RF),(R1),,ELEM,4,GLVXACT                                        
*                                                                               
         MVC   ELEM(8),=C'NOW,PDF'                                              
         GOTO1 (RF),(R1),,ELEM,7,GLVSPRNT                                       
*                                                                               
         GOTO1 (RF),(R1),,QCLTA,3,GLVSPCLT                                      
*                                                                               
         GOTO1 (RF),(R1),,QNETA,4,GLVSPSTA                                      
*                                                                               
         GOTO1 (RF),(R1),,QPRG,6,GLVSPPRG                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         GOTO1 VGLOBBER,DMCB,=C'PUTD',QPER,8,GLVSPPER                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING GLVTRFD,R4                                                       
         LA    R0,M#DOWNNI         RETURN TO ROUTINE AFTER XCTL                 
         MVC   TRFACT,=C'BLD'                                                   
         CLI   BYTE,C'B'                                                        
         JE    QNGLOB2                                                          
         LA    R0,M#ENDNI                                                       
         MVC   TRFACT,=C'SUB'                                                   
*                                                                               
QNGLOB2  STH   R0,HALF                                                          
         GOTO1 VHEXOUT,DMCB,(2,HALF),WORK,2,=C'TOG'                             
         MVC   TRFROUT,WORK+1      3 CHAR HEX CODE IN EBCDIC                    
*                                                                               
         MVC   TRFFAX,QOPTFAX                                                   
         MVC   TRFDOALL,QOPTALL                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,24,GLVSPTRF                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',QOPTIONS,60,GLVBUY1                       
*                                                                               
         GOTO1 (RF),(R1),,QUNIQID,60,GLVBUY2                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING GLVXFRSY,R4                                                      
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'NNA'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
         MVC   GLVXTOPR,=C'TRA'                                                 
         OI    GLVXFLG1,GLV1SEPS+GLV1SEPD                                       
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
         J     EXITY                                                            
         DROP  R4                                                               
*                                                                               
       ++INCLUDE DDGLVSPTRF                                                     
SVRDEF   CSECT ,                                                                
                                                                                
*==========================================================                     
* CHECK FOR AN ERROR RETURN FROM TRAFFIC                                        
*==========================================================                     
         EJECT                                                                  
GLOBALS  DS    0D                                                               
         LTORG                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE NENAVWORKD                                                     
         PRINT ON                                                               
                                                                                
WORKD    DSECT ,                   ** Redefine OVERWORK **                      
         ORG   OVERWORK                                                         
RUNMODE  DS    CL1                                                              
*                                                                               
         DS    0D                                                               
SVNEXT   DS    A                                                                
QVALUES  DS    0C                                                               
QRUNALL  DS    CL1                                                              
QNETA    DS    CL4                                                              
QPRG     DS    CL6                                                              
QPER     DS    CL8                                                              
QOPTFAX  DS    CL1                                                              
QOPTALL  DS    CL1                                                              
QUNIQID  DS    CL60                                                             
QERRCODE DS    CL4                                                              
QOPTIONS DS    CL60                                                             
         ORG   QOPTIONS                                                         
QERRTEXT DS    CL69                                                             
QVALUEX  EQU   *                                                                
*                                                                               
* OUTPUT FIELDS FROM PROGRAM TABLE                                              
*                                                                               
NIPROG   DS    CL6                                                              
NICURREV DS    CL11                                                             
NIPRVREV DS    CL11                                                             
NIUNITS  DS    CL3                                                              
NIFEEDS  DS    CL3                                                              
NINOCML  DS    CL3                                                              
NIUNAL   DS    CL3                                                              
NIDEL    DS    CL3                                                              
*                                                                               
         DS    0A                                                               
PRGNUMI  DS    XL1                                                              
APRGLST  DS    AL3                                                              
*                                                                               
SAVED    DSECT ,                   ** DSECT TO COVER SAVED STORAGE **           
WVALUES  DS    0X                  ** LITERAL VALUES **                         
VS081000 DS    XL4                 PC VERSION 0.8.10.0                          
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
       ++INCLUDE SPPRGTAB                                                       
*                                                                               
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DDGLVXCTLD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NENAV40   05/02/16'                                      
         END                                                                    
