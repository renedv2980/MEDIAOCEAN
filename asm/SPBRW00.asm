*          DATA SET SPBRW00    AT LEVEL 041 AS OF 03/25/03                      
*PHASE T22400A,*                                                                
*INCLUDE REGENWLD                                                               
         TITLE 'SPBRW00 - T22400 - SPOT RECORD BROWSE/SEARCH FACILITY'          
*********************************************************************           
* PROGRAMMER NOTE: DOCUMENTATION FOR THE USE AND MAINTENANCE OF     *           
* THIS PROGRAM IS PROVIDED IN REBRWTXT                              *           
*********************************************************************           
T22400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWORK,*T22400*,R9,RR=R5,CLEAR=YES                                
*                                                                               
         USING GENOLD,RC                                                        
*                                                                               
         ST    R5,RELO             ROOT PHASE RELO FACTOR                       
         ST    R1,SYSFAC           SAVE A(FACILITIES)                           
         L     RE,16(R1)           A(COMFACS)                                   
         ST    RE,ACOMFACS                                                      
         BAS   RE,INITL                                                         
         USING T224FFD,RA                                                       
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RE,CGLOBBER                                                      
         ST    RE,GLOBBER          SAVE A(GLOBBER)                              
         L     RE,CGETTXT                                                       
         ST    RE,GETTXT           SAVE A(GETTXT)                               
         L     RE,CDATCON                                                       
         ST    RE,DATCON           SAVE A(DATCON)                               
         DROP  RF                                                               
*                                                                               
***********************************************************************         
* MAIN PROGRAM                                                        *         
***********************************************************************         
*                                                                               
         LA    R5,BRWSEL1H                                                      
         ST    R5,PTRREC           SAVE POINTER TO 1ST SELECT FIELD             
         MVI   LINENUM,1           INITIALIZE LINE COUNTER                      
*                                                                               
         TM    STAT,NOTFIRST       FIRST TIME IN PROGRAM?                       
         BO    MAIN010             NO - SKIP                                    
         XC    BRWMSG1,BRWMSG1                                                  
         MVC   BRWMSG1(L'MSGCODE),MSGCODE   PROMPT FOR RECORD CODE              
         OI    BRWMSG1H+6,X'80'                                                 
MAIN010  DS    0H                                                               
*                                                                               
         TM    STAT,NOTFIRST       FIRST TIME IN PROGRAM?                       
         BO    MAIN020             NO - SKIP                                    
         BAS   RE,SETUP            YES - DO SETUP ROUTINE                       
*                                                                               
         TM    STAT,BYGLOB         FILE SWITCHING ONLY IF FROM GLOBBER          
         BZ    MAIN020                                                          
         CLI   SRCEUTL,0           REQUEST FOR FILE SWITCH?                     
         BE    MAIN020             NO                                           
         CLI   ORIGUTL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                MUST PROVIDE ORIGINAL FILE                   
         OC    SRCEAGY,SRCEAGY                                                  
         BNZ   *+6                                                              
         DC    H'0'                MUST PROVIDE SOURCE AGY CODE                 
         OI    STAT,SWITCH                                                      
         MVC   CURAGY,SRCEAGY      USE SOURCE AGY                               
         BAS   RE,SETFIL           SWTICH FILE                                  
*                                                                               
MAIN020  DS    0H                                                               
         TM    STAT,BYGLOB         ONLY TRAP PFKEYS IF FROM GLOBBER             
         BZ    MAIN030                                                          
         L     R1,SYSFAC           CHECK FOR CANCEL PF KEY                      
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
         CLI   TIOBAID,12          PF12?                                        
         BE    *+12                                                             
         CLI   TIOBAID,24          PF24?                                        
         BNE   *+12                NO - SKIP                                    
         OI    STAT,CANCEL         YES - SET CANCEL FLAG                        
         B     RETURN                    AND RETURN TO PROG OR EXIT             
         DROP  RF                                                               
*                                                                               
MAIN030  DS    0H                                                               
         TM    STAT,BYGLOB         CALLED BY GLOBBER?                           
         BZ    *+12                NO - NO SELECT FIELDS TO CHECK               
         BAS   RE,CKSEL            SEE IF REC IS SELECTED                       
         BE    RETURN              PASS RECORD BACK TO APPLICATION              
*                                                                               
         BAS   RE,CLRSCRN          CLEAR SELECT LINES                           
*                                                                               
         LA    R2,BRWKEYWH         KEYWORD FIELD                                
         OC    BRWKEYW,SPACES                                                   
         CLC   KEYWORD,8(R2)       MODIFIED?                                    
         BE    *+14                                                             
         MVC   KEYWORD,8(R2)                                                    
         NI    STAT,X'FF'-CONTINUE                                              
*                                                                               
         LA    R2,BRWRECH          RECORD CODE FIELD                            
         LA    R3,1                                                             
         CLI   5(R2),0                                                          
         BE    MYERR                                                            
         CLC   REC,8(R2)           REC TYPE CHANGED?                            
         BE    MAIN050                                                          
         NI    STAT,X'FF'-CONTINUE                                              
         LA    R3,630                                                           
         BAS   RE,CKREC            LOOKUP NEW REC TYPE                          
         BNE   MYERR                                                            
         MVC   REC,8(R2)                                                        
*                                                                               
MAIN050  DS    0H                                                               
         XC    BRWMSG1,BRWMSG1                                                  
         MVI   OPT,0               CLEAR OPTIONS FLAG                           
*                                                                               
         LA    R2,BRWOPTH          OPTIONS FIELD HEADER                         
*                                                                               
         CLC   REC,=C'LSP'                                                      
         BNE   MAIN060                                                          
         OI    OPT,OPTSPER                                                      
         CLI   5(R2),0                                                          
         BE    MAIN080                                                          
         CLC   8(6,R2),=C'SALESP'                                               
         BE    MAIN080                                                          
         CLC   8(6,R2),=C'POINTP'                                               
         BE    ORDRLNKD                                                         
         B     ERROPT1                                                          
*                                                                               
MAIN060  CLC   REC,=C'LPP'                                                      
         BNE   MAIN070                                                          
         OI    OPT,OPTPPER                                                      
         CLI   5(R2),0                                                          
         BE    MAIN080                                                          
         CLC   8(6,R2),=C'POINTP'                                               
         BE    MAIN080                                                          
         CLC   8(6,R2),=C'SALESP'                                               
         BE    ORDRLNKD                                                         
         B     ERROPT1                                                          
*                                                                               
MAIN070  TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BO    *+8                 YES                                          
         NI    STAT,X'FF'-CONTINUE DO NOT CONTINUE LIST WITH PREV FLAG          
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   MAIN078             YES                                          
*                                                                               
         XC    BRWOPT,BRWOPT                                                    
         CLC   REC,=C'BSP'                                                      
         BNE   MAIN075                                                          
         MVC   BRWOPT(6),=CL6'SALESP'                                           
         B     MAIN077                                                          
*                                                                               
MAIN075  CLC   REC,=C'BPP'                                                      
         BNE   ERROPT2             ERROR, MUST ENTER OPTION FILTER              
         MVC   BRWOPT(6),=CL6'POINTP'                                           
MAIN077  MVI   5(R2),6                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
MAIN078  BAS   RE,VALOPT           VALIDATE THE OPTIONS                         
         BNE   XIT                 DISPLAY ERROR MESSAGE                        
*                                                                               
MAIN080  OI    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         TM    STAT,NOTFIRST       FIRST TIME IN PROGRAM?                       
         BO    MAIN085             NO - SKIP                                    
*                                                                               
         TM    STAT,BYGLOB         HERE FROM GLOBBER?                           
         BZ    *+14                                                             
         MVC   BRWMSG1(L'MSGDISP),MSGDISP  YES - OFFER SELECT TO USERS          
         B     *+10                                                             
         MVC   BRWMSG1(L'MSGDIS2),MSGDIS2  NO - DON'T OFFER SELECT              
         OI    BRWMSG1H+6,X'80'                                                 
*                                                                               
MAIN085  ICM   RF,15,RECROUTE      RECORD HANDLING ROUTINE                      
         A     RF,RELO                                                          
         BASR  RE,RF               GO HANDLE RECORD TYPE                        
*                                                                               
         XC    BRWMSG1,BRWMSG1     CLEAR THE MESSAGE FIELD                      
         TM    STAT,CONTINUE       WILL DISPLAY BE CONTINUING?                  
         BZ    MAIN090                                                          
         MVC   BRWMSG1(L'MSGCNT),MSGCNT YES - TELL USER TO HIT ENTER            
         B     MAIN100             AND GO WRAP IT UP AND LEAVE                  
*                                                                               
MAIN090  MVC   BRWMSG1(L'MSGNONE),MSGNONE                                       
         CLI   LINENUM,1           ANY RECORDS DISPLAYED?                       
         BE    *+10                YES                                          
         MVC   BRWMSG1(L'MSGEOR),MSGEOR  ALL RECORDS DISPLAYED MSG              
*                                                                               
MAIN100  DS    0H                                                               
         OI    BRWMSG1H+6,X'80'    XMIT MSG1 FIELD                              
         OI    BRWSERVH+1,X'01'    MAKE SERVICE REQUEST MODIFIED                
         OI    BRWSERVH+6,X'80'                                                 
         OI    BRWKEYWH+6,X'40'+X'80' SET CURSOR TO KEYWORD FIELD               
*                                                                               
         OI    STAT,NOTFIRST       NOT ANYMORE                                  
         B     EXXMOD                                                           
***********************************************************************         
* RETURN - BUILD GLOBBER CONTROL ELEM FOR RETURN & EXIT PROGRAM      *          
***********************************************************************         
RETURN   DS    0H                                                               
         TM    STAT,BYGLOB         SHOULD ONLY BE RETURNING BY                  
         BO    *+6                 GLOBBER IF WE WERE CALLED BY GLOBBER         
         DC    H'0'                                                             
*                                                                               
         TM    STAT,SWITCH         HAVE WE DONE A FILE SWITCH?                  
         BZ    *+8                 NO                                           
         BAS   RE,ORIGFIL          YES - SWITCH BACK TO ORIGINAL FILE           
*                                                                               
         TM    STAT,CANCEL         CANCEL REQUEST?                              
         BZ    RTN005                                                           
         BAS   RE,CKBAD                                                         
         TM    STAT,BAD                                                         
         BO    RTN005                                                           
         LA    R5,GLOBELEM                                                      
         USING GLBRWKW,R5                                                       
         MVC   GLBRWREC,=C'CAN'                                                 
         XC    GLBRWKW,GLBRWKW                                                  
         GOTO1 GLOBBER,DMCB,=C'PUTD',(R5),GLBRWLNQ,GLRBRWSE                     
         CLI   8(R1),0                                                          
         BE    RTN010                                                           
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
RTN005   DS    0H                                                               
         TM    STAT,BAD                                                         
         BZ    RTN010                                                           
         LA    R5,GLOBELEM         DO A BAD RETURN TO CALLER                    
         USING GLBRWKW,R5                                                       
         MVC   GLBRWREC,=C'BAD'                                                 
         XC    GLBRWKW,GLBRWKW                                                  
         GOTO1 GLOBBER,DMCB,=C'PUTD',(R5),GLBRWLNQ,GLRBRWSE                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
RTN010   DS    0H                                                               
         LA    R4,GLCNTRL                                                       
         USING GLVXFRSY,R4                                                      
         MVC   WORK(6),GLVXFRSY    BUFFER 'FROM' SYS & PGM                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'BRO'                                                 
         MVC   GLVXTOSY(6),WORK    ORIGINAL 'FROM' SYS & PGM                    
         MVI   GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         GOTO1 GLOBBER,DMCB,=C'PUTD',(R4),GLVXLENQ,GLVXCODQ                     
         CLI   8(R1),0             GLOBBER ERRORS?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    STAT,X'FF'-BYGLOB   AFTER WE LEAVE, NO LONGER BY GLOB            
         OI    STAT,NOTFIRST       NOT ANYMORE                                  
         B     EXXMOD                                                           
         DROP  R4                                                               
***********************************************************************         
* CKBAD - MAKE SURE BROWSE ELEM IS STILL HERE & DELETE IT                       
***********************************************************************         
CKBAD    NTR1                                                                   
         TM    STAT,BYGLOB                                                      
         BO    *+6                                                              
         DC    H'0'                                                             
*                                  DELETE THE BROWSE ELEMENT HERE               
         GOTO1 GLOBBER,DMCB,=C'DELE',0,GLBRWLNQ,GLRBRWSE                        
         CLI   8(R1),0             BROWSE ELEM STILL AROUND?                    
         BE    *+8                 YES - GOOD                                   
         OI    STAT,BAD            NO - BAD                                     
         B     EXXMOD                                                           
*                                                                               
***********************************************************************         
* CKREC - LOOKP RECORD TYPE IN TABLE                                 *          
***********************************************************************         
CKREC    NTR1                                                                   
         LA    R4,RECTAB                                                        
CKREC010 DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    NOTEQUAL                                                         
         CLC   BRWREC,0(R4)                                                     
         BE    CKREC020                                                         
         LA    R4,L'RECTAB(R4)                                                  
         B     CKREC010                                                         
CKREC020 DS    0H                                                               
         MVC   RECDESC,3(R4)                                                    
         MVC   RECROUTE,18(R4)                                                  
         MVC   BRWDESC,RECDESC                                                  
         OI    BRWDESCH+6,X'80'                                                 
         B     EQUAL                                                            
***********************************************************************         
* DISPREC - DISPLAY RECORD & BUFFER KEYFIELD                          *         
***********************************************************************         
DISPREC  NTR1                                                                   
         CLI   LINENUM,LINES                                                    
         BNH   DR050                                                            
         OI    STAT,CONTINUE                                                    
         MVC   LASTKEY,GEKEY                                                    
         B     NOTEQUAL                                                         
DR050    DS    0H                                                               
         L     R2,PTRREC                                                        
         TM    STAT,BYGLOB                                                      
         BZ    DR060                                                            
         MVI   8(R2),C'_'                                                       
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
DR060    DS    0H                                                               
         ZIC   R3,0(R2)                                                         
         AR    R3,R2                                                            
         MVC   8(L'RECLINE,R3),RECLINE                                          
         OI    6(R3),X'80'                                                      
         LA    R2,BRWSEL2H-BRWSEL1H(R2)                                         
         ST    R2,PTRREC                                                        
         ZIC   R3,LINENUM                                                       
         LA    R3,1(R3)                                                         
         STC   R3,LINENUM                                                       
         B     EQUAL                                                            
***********************************************************************         
* SALREC - PROCESS SAL RECORDS (SALESPERSON)                          *         
***********************************************************************         
SALREC   NTR1                                                                   
*                                                                               
         XC    GEKEY,GEKEY                                                      
         LA    R4,GEKEY                                                         
*                                                                               
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    SAL005              NO                                           
         MVC   GEKEY,LASTKEY       NEXT RECORD TO LIST                          
         NI    STAT,X'FF'-CONTINUE TURN OFF CONTINUE FLAG                       
         B     SAL020              READHI                                       
*                                                                               
         USING GSPLKEY,R4          SALESPERSON KEY DSECT                        
SAL005   MVI   GSPLKTYP,GSPLRECQ   X'71'                                        
         OC    ALPHAID,ALPHAID     ALPHA ID FROM GLOBBER?                       
         BZ    SAL010              NO                                           
         MVC   MALPHAID,ALPHAID                                                 
         MVC   GSPLKREP,ALPHAID    SUBSIDIARY REP CODE                          
         BAS   RE,GENHIGH                                                       
         CLC   GEKEY(6),GEKEYSV    FOUND IT?                                    
         BNE   SAL010              NO                                           
         MVC   MALPHAID,GSPLMREP   YES, MOVE IN MASTER REP CODE                 
         DROP  R4                                                               
*                                                                               
         USING GSPLPKEY,R4         PASSIVE KEY DSECT                            
SAL010   XC    GEKEY,GEKEY                                                      
         LA    R4,GEKEY                                                         
         MVI   GSPLPTYP,GSPLPTYQ   X'F1'                                        
         MVI   GSPLPSTP,X'01'      SALESPERSON                                  
         MVC   GSPLPREP,MALPHAID   ALPHA ID                                     
         TM    OPT,OPTPPER         FILTER ON POINTPERSON?                       
         BZ    SAL020              NO                                           
         MVI   GSPLKSTP,X'02'      POINTPERSON                                  
*                                                                               
SAL020   BAS   RE,GENHIGH          READ GENDIR HIGH                             
         B     SAL040                                                           
SAL030   DS    0H                                                               
         BAS   RE,GENSEQ           READ GENDIR SEQ                              
SAL040   DS    0H                                                               
         LA    R5,3                COMPARE FOR 4 BYTES                          
         OC    MALPHAID,MALPHAID   HAVE AN ID FROM GLOBBER?                     
         BZ    *+8                 NO                                           
         LA    R5,5                YES, COMPARE FOR 6                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   GEKEY(0),GEKEYSV    DONE WITH SALEPERSON/POINTPERSON?            
         BE    SAL045              NO                                           
*                                                                               
         TM    OPT,OPTSPER+OPTPPER FILTERED ON SALESPERSON/POINTPERSON?         
         BNZ   SALX                YES, MUST BE DONE                            
*                                                                               
         CLI   GEKEYSV+3,X'01'     WERE WE JUST LISTING SALESPERSON?            
         BNE   SALX                NO, DONE                                     
*                                                                               
         CLC   GEKEY(3),=X'0000F1' HAVE S/P RECORD?                             
         BNE   SALX                NO, DONE                                     
*                                                                               
         XC    GEKEY,GEKEY                                                      
         MVI   GEKEY+2,X'F1'                                                    
         MVI   GEKEY+3,X'02'       FIND ALL POINTPERSONS                        
         MVC   GSPLPREP,MALPHAID   WITH THIS ALPHA ID                           
         B     SAL020                                                           
*                                                                               
SAL045   CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    SAL050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(20,GSPLPNAM),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    SAL030                                                           
*                                                                               
SAL050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD     DID WE ALREADY DISPLAY THE HEADING?          
         BNZ   SAL060              YES                                          
*                                                                               
         MVC   BRWHEAD(16),=C'SALESPERSON NAME'                                 
         MVC   BRWHEAD+22(6),=C'OFFICE'                                         
         MVC   BRWHEAD+33(12),=C'POINTPERSON?'                                  
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(16),DASHES                                                
         MVC   BRWUL2+22(6),DASHES                                              
         MVC   BRWUL2+33(12),DASHES                                             
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
SAL060   XC    RECLINE,RECLINE                                                  
         LA    R2,RECLINE                                                       
         USING LINED,R2                                                         
         OC    OFFICE,OFFICE       OFFICE PASSED VIA GLOBBER?                   
         BZ    SAL065              NO                                           
         CLC   GSPLPOFF,OFFICE     MATCH ON OFFICE?                             
         BNE   SAL030              NO                                           
*                                                                               
SAL065   MVC   LINSALN,GSPLPNAM    MOVE SALESPERSON TO SCREEN                   
         MVC   LINOFF,GSPLPOFF     OFFICE TO SCREEN                             
*                                                                               
         CLI   GSPLPSTP,X'01'      SALESPERSON?                                 
         BE    *+8                 YES                                          
         MVI   LINPP,C'Y'          POINTPERSON = Y TO SCREEN                    
*                                                                               
         CLI   1(RA),C'*'          DDS TERMNIAL?                                
         BNE   SAL070                                                           
         MVC   LINREP,GSPLPREP                                                  
         MVC   LINSALC,GSPLPCOD                                                 
*                                                                               
SAL070   BAS   RE,GENGET                                                        
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING GSPLSPCD,R6                                                      
*                                                                               
         CLI   1(R6),GSPLSALL                                                   
         BL    SAL090                                                           
         CLC   GSPLSPPC,SPACES                                                  
         BNH   SAL090                                                           
         TM    GSPLSPFL,X'10'      POWER CODE MINUS?                            
         BZ    SAL075                                                           
* YES, TO MINUS, SHOW SALESPEOPLE NOT OF THIS REP                               
         CLC   GSPLSPPC,ALPHAID                                                 
         BE    SAL030                                                           
         B     SAL090                                                           
* NO, TO MINUS, SHOW SALESPEOPLE OF THIS REP ONLY                               
SAL075   CLC   GSPLSPPC,ALPHAID                                                 
         BNE   SAL030                                                           
*                                                                               
SAL090   BAS   RE,DISPREC          DID WE DISPLAY THE RECORD?                   
         BE    SAL030              YES, READ NEXT REC                           
*                                                                               
SALX     B     XIT                                                              
         DROP  R4,R2,R6                                                         
         EJECT                                                                  
***********************************************************************         
* CLRSCRN - CLEAR RECORD DISPLAY/SELECT SCREEN FIELDS                 *         
***********************************************************************         
CLRSCRN  NTR1                                                                   
         LA    R2,BRWSEL1H                                                      
         LA    R3,BRWREC1H                                                      
         LA    R4,LINES                                                         
CLR010   DS    0H                                                               
         MVI   8(R2),0             CLEAR SELECT FIELD                           
         OI    1(R2),X'20'         PROTECT SELECT FIELD                         
         OI    6(R2),X'80'                                                      
         XC    8(L'BRWREC1,R3),8(R3)  CLEAR RECORD FIELD                        
         OI    6(R3),X'80'                                                      
         LA    R2,BRWSEL2H-BRWSEL1H(R2)                                         
         LA    R3,BRWREC2H-BRWREC1H(R3)                                         
         BCT   R4,CLR010                                                        
         XC    BRWMSG1,BRWMSG1     CLEAR MESSAGE 2                              
         OI    BRWMSG1H+6,X'80'                                                 
         XC    BRWHEAD,BRWHEAD                                                  
         OI    BRWHEADH+6,X'80'                                                 
         XC    BRWUL2,BRWUL2                                                    
         OI    BRWUL2H+6,X'80'                                                  
         LA    R2,BRWSELLH         SELECT FIELDS HEADER                         
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
         LA    R2,BRWUL1H          SEL HEADER UNDERLINE                         
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
* CKSEL - CHECK SELECT FIELDS, IF ONE SELECTED, RETURN TO CALLER      *         
***********************************************************************         
CKSEL    NTR1                                                                   
         LA    R2,BRWSEL1H                                                      
         LA    R5,LINES                                                         
CKSEL010 DS    0H                                                               
         TM    1(R2),X'20'         HIT A PROTECTED SELECT FIELD?                
         BO    NOTEQUAL            YES - EXIT                                   
         CLI   8(R2),C'S'          SELECTED?                                    
         BE    CKSEL030            YES - GRAB RECORD & RETURN TO CALLER         
         CLI   8(R2),0             VALIDATE SELECT FIELD                        
         BE    CKSEL020                                                         
         LA    R3,2                                                             
         CLI   8(R2),C'_'                                                       
         BNE   MYERR                                                            
CKSEL020 DS    0H                                                               
         LA    R2,BRWSEL2H-BRWSEL1H(R2) NEXT SELECT FIELD                       
         BCT   R5,CKSEL010         LOOP                                         
         B     NOTEQUAL            YES - EXIT                                   
CKSEL030 DS    0H                                                               
         BAS   RE,CKBAD            CHECK IF OK TO DO RETURN                     
         TM    STAT,BAD                                                         
         BO    EQUAL                                                            
         LA    R5,GLOBELEM                                                      
         USING GLBRWKW,R5                                                       
         LR    R4,R2                                                            
         USING BRWSEL1H,R4                                                      
         LA    R2,BRWREC1                                                       
         DROP  R4                                                               
         USING LINED,R2                                                         
         MVC   GLBRWKW,LINSALN     MOVE BUFFER ENTRY INTO GLOBBER ELEM          
         MVC   GLBRWAPG,LINSALC    MOVE BUFFER ETNRY INTO GLOBBER ELEM          
         CLI   LINPP,C'Y'     POINTPERSON?                                      
         BNE   *+8                                                              
         OI    GLBRWFLG,X'80'      POINTPERSON FLAG FOR SPOMS09/0A              
         DROP  R2                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',(R5),GLBRWLNQ,GLRBRWSE                     
         CLI   8(R1),0                                                          
         BE    EQUAL                                                            
         DC    H'0'                                                             
         DROP  R5                                                               
***********************************************************************         
* SELHEAD - DISPLAY SELECT COLUMN HEADER                              *         
***********************************************************************         
SELHEAD  NTR1                                                                   
         TM    STAT,BYGLOB                                                      
         BZ    XIT                                                              
         LA    R2,BRWSELLH         SELECT FIELDS HEADER                         
         NI    1(R2),X'FF'-X'0C'   TURN OFF ZERO INTENSITY                      
         OI    6(R2),X'80'                                                      
         LA    R2,BRWUL1H          SEL HEADER UNDERLINE                         
         NI    1(R2),X'FF'-X'0C'   TURN OFF ZERO INTENSITY                      
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
* SETUP - CHECK FOR GLOBBER ELEM, VALIDATE SIGN ON                    *         
***********************************************************************         
SETUP    NTR1                                                                   
*                                                                               
         MVI   OPT,0               DEFAULT NO OPTIONS (FOR OPT FIELD)           
         MVI   OPTIONS,0           DEFAULT NO OPTIONS                           
         MVC   CURAGY,REPALPHA     DEFAULT CURRENT AGY                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,TODAY)                                      
*                                                                               
         LA    R4,GLCNTRL                                                       
         USING GLVXFRSY,R4                                                      
*                                  LOOK FOR GLOBBER CONTROL ELEM                
         GOTO1 GLOBBER,DMCB,=C'GETD',(R4),GLVXLENQ,GLVXCODQ                     
         TM    8(R1),GLEGNF        HAVE GLOBBER CONTROL ELEM?                   
         BO    XIT                 GET OUT OF HERE                              
         CLI   8(R1),0             OTHER GLOBBER ERRORS?                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GLOBBER,DMCB,=C'DELE',0,GLVXLENQ,GLVXCODQ                        
         CLI   8(R1),0             GLOBBER ERRORS?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         OI    STAT,BYGLOB         SET CALLED BY GLOBBER FLAG                   
*                                                                               
         LA    R4,GLOBELEM                                                      
         USING GLBRWKW,R4                                                       
*                                                                               
*                                  LOOK FOR BROWSE GLOBBER ELEM                 
         GOTO1 GLOBBER,DMCB,=C'GETD',(R4),GLBRWLNQ,GLRBRWSE                     
         CLI   8(R1),0             MUST HAVE GLOBBER BROWSE ELEM                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   OPTIONS,GLBRWFLG    OPTIONS BITS FOR RECORD FORMATS              
         OC    GLBRWADV,GLBRWADV                                                
         MVC   ALPHAID,GLBRWADV    2-BYTE AGENCY ID                             
         MVC   OFFICE,GLBRWADV+2   OFFICE CODE                                  
*                                                                               
*                                  (THESE ARE FOR FILE SWITCHING ONLY)          
         MVC   SRCEUTL,GLBRWSF     SOURCE FILE                                  
         MVC   ORIGUTL,GLBRWOF     ORIGINAL FILE                                
         MVC   SRCEAGY,GLBRWSR     SOURCE AGY                                   
*                                                                               
         LA    R2,BRWRECH          RECORD CODE FIELD IS FOR DDS USE             
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
         LA    R2,BRWRECLH         RECORD CODE FIELD LABEL                      
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
         LA    R2,BRWPFKLH         PFKEY LABEL                                  
         NI    1(R2),X'FF'-X'0C'   TURN OFF ZERO INTENSITY                      
         OI    6(R2),X'80'                                                      
         LA    R2,BRWRECH          YES - MOVE RECORD TYPE FROM GLOBBER          
         MVC   8(3,R2),GLBRWREC          ELEM INTO RECTYPE FIELD                
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         OC    GLBRWKW(9),SPACES   FILTER ON SALESPERSON NAME?                  
         BZ    SETUP10             NO                                           
         LA    R2,BRWKEYWH         YES - MOVE KEYWORD FROM GLOBBER              
         MVC   8(9,R2),GLBRWKW         ELEM INTO RECTYPE FIELD                  
***                                                                             
* MAKE IT A WILD CARD                                                           
***                                                                             
         LA    R1,17(R2)           LAST SPOT FOR A CHARACTER                    
*                                                                               
SETUP01  CLI   0(R1),X'40'         IS THERE A CHARACTER THERE?                  
         BH    SETUP05             YES                                          
         BCTR  R1,0                NO, CHECK SPACE BEFORE                       
         B     SETUP01                                                          
*                                                                               
SETUP05  MVI   1(R1),C'*'          MOVE WILDCARD IN                             
*                                                                               
SETUP10  MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         MVC   BRWSERV(4),=C'=RE '                                              
         OI    BRWSERVH+6,X'80'                                                 
         DROP  R4                                                               
         B     XIT                                                              
***********************************************************************         
*   ORIGFIL:   SWITCHES TO ORIGINAL SPOT FILE IF NECESSARY                      
***********************************************************************         
ORIGFIL  NTR1                                                                   
         MVC   CURAGY,REPALPHA                                                  
         NI    STAT,X'FF'-SWITCH                                                
         CLC   SRCEUTL,ORIGUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    XIT                 YES - DON'T SWITCH                           
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    XIT                                                              
         DC    H'0'                NO  - ABORT                                  
***********************************************************************         
*   SETFIL:    SWITCHES TO SOURCE SPOT FILE, IF NECESSARY.                      
***********************************************************************         
SETFIL   NTR1                                                                   
         CLC   SRCEUTL,ORIGUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    XIT                 YES - DON'T SWITCH                           
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(SRCEUTL,X'FFFFFFFF'),0                                
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    XIT                                                              
         DC    H'0'                                                             
***********************************************************************         
*   VALIDATE OPTIONS FIELD                                                      
***********************************************************************         
VALOPT   NTR1                                                                   
*                                                                               
         LA    R2,BRWOPTH          OPTIONS FIELD HEADER                         
         ZIC   R1,5(R2)            INPUT FIELD LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SALESP'                                               
         BE    VOPT2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'POINTP'                                               
         BNE   ERROPT1                                                          
*                                                                               
VOPT1    OI    OPT,OPTPPER                                                      
         B     VOPTX                                                            
*                                                                               
VOPT2    OI    OPT,OPTSPER                                                      
*                                                                               
VOPTX    B     EQUAL                                                            
***********************************************************************         
*   INITIALIZE STUFF                                                            
***********************************************************************         
INITL    DS    0H                  SET UP TO CLEAR WORK SPACE                   
         LR    R0,RE                                                            
         LM    R2,R4,0(R1)                                                      
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(40),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   REPALPHA,14(RA)          REP CODE                                
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    RE,IOAREA           PRESET IOAREA ADDRESS                        
         ST    RE,AIOAREA                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        GENDIR/GENFIL DATAMGR CALLS                                  *         
***********************************************************************         
GENHIGH  MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   GEKEYSV,GEKEY                                                    
         B     GENLINK                                                          
*                                                                               
GENSEQ   MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     GENLINK                                                          
*                                                                               
GENLINK  NTR1                                                                   
         LA    R4,IOAREA                                                        
         GOTO1 VDATAMGR,DMCB,COMMAND,=C'GENDIR',GEKEY,(R4),(0,DMWORK)           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   GEKEY,IOAREA                                                     
         XIT1                                                                   
*                                                                               
GENGET   NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=CL8'GETREC',=C'GENFIL',                  +        
               GEKEY+36,IOAREA,(0,DMWORK),0                                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RECORD TYPE TABLE                                                   *         
***********************************************************************         
*   BYTE 0-2  = 3 LETTER RECORD CODE                                            
*                                                                               
*   BYTE 3-17 = 15 CHARACTER RECORD TYPE DESCRIPTION TO BE DISPLAYED            
*                                                                               
*   BYTE 18-21= A(ROUTINE TO HANDLE READING OF RECORDS)                         
*                                                                               
RECTAB   DS    0CL22                                                            
         DC    C'SAL',CL15'SALES PERSON',AL4(SALREC)                            
         DC    C'LSP',CL15'SALES PERSON',AL4(SALREC)                            
         DC    C'LPP',CL15'POINT PERSON',AL4(SALREC)                            
         DC    C'BSP',CL15'SALES PERSON',AL4(SALREC)                            
         DC    C'BPP',CL15'POINT PERSON',AL4(SALREC)                            
         DC    X'FF'                                                            
***********************************************************************         
*                                                                               
NOTEQUAL LTR   RB,RB                                                            
         B     XIT                                                              
EQUAL    CR    RB,RB                                                            
XIT      XIT1                                                                   
EXXMOD   XMOD1 1                                                                
***********************************************************************         
*                     VARIOUS MESSAGES                                *         
***********************************************************************         
MSGCNT   DC    C'More records available. Press ENTER to continue.'              
MSGEOR   DC    C'Records displayed. End of list.'                               
MSGNONE  DC    C'No matching records found.'                                    
MSGCODE  DC    C'SPOTPAK RECORD BROWSER - ENTER RECORD CODE + KEYWORD'          
MSGDISP  DC    C'SPOTPAK RECORD BROWSER - select record or change keywo+        
               rd'                                                              
MSGDIS2  DC    C'SPOTPAK RECORD BROWSER - View records or change keywor+        
               d'                                                               
SPACES   DC    CL80' '                                                          
DASHES   DC    80C'-'                                                           
***********************************************************************         
*                     VARIOUS ERROR MESSAGES                          *         
***********************************************************************         
MYERR    DS    0H                                                               
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         OI    STAT,NOTFIRST       NOT ANYMORE                                  
         L     RD,BASERD                                                        
         LTR   R3,R3               IF DATAMGR ERROR, DON'T SET CURPOS           
         BZ    XIT                                                              
         OI    6(R2),X'40'                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
ERROPT1  MVC   BRWMSG1(40),=CL40'Valid options are SALESP and POINTP'           
         B     ERROPTX                                                          
*                                                                               
ERROPT2  MVC   BRWMSG1(52),MISSOPT                                              
         B     ERROPTX                                                          
MISSOPT  DC    C'Enter Salesp or Pointp, or update your Buyer record.'          
*                                                                               
ORDRLNKD MVC   BRWMSG1(49),ORDRLNK                                              
         CLC   REC,=C'LPP'                                                      
         BNE   ERROPTX                                                          
         MVC   BRWMSG1+37(5),=CL5'Point'                                        
         B     ERROPTX                                                          
ORDRLNK  DC    C'Contract is linked - please select a Salesperson.'             
*                                                                               
ERROPTX  LA    R2,BRWOPTH                                                       
         OI    6(R2),X'40'         POSITION CURSOR ON THIS FIELD                
         OI    BRWMSG1H+6,X'80'    TRANSMIT                                     
         B     NOTEQUAL            SET CC NOT EQUAL                             
*                                                                               
         GETEL R6,42,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
* RGENOLD                                                                       
* FATWA                                                                         
* FATIOB                                                                        
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* REGLBRW                                                                       
* SPGENREP                                                                      
* GEGENSPSAL                                                                    
* DDCOMFACS                                                                     
* SPBRWFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE RGENOLD                                                        
         PRINT ON                                                               
         ORG   IOAREA+1008                                                      
BASERB   DS    F                                                                
BASERD   DS    F                                                                
RELO     DS    A                   RELOCATION FACTOR - ROOT PHASE               
SYSFAC   DS    A                   A(SYSTEM FACILITIES)                         
ACOMFACS DS    A                   A(COMFACS)                                   
GLOBBER  DS    A                   A(GLOBBER)                                   
GETTXT   DS    A                   A(GETTXT)                                    
DATCON   DS    A                   A(DATCON)                                    
PTRREC   DS    A                   NEXT AVAILABLE REC DISPLAY LINE              
LINENUM  DS    X                   CURRENT DISPLAY LINE NUMBER                  
ELCODE   DS    C                                                                
*                                                                               
GLOBELEM DS    CL(GLBRWLNQ)        BROWSE ELEMENT FROM GLOBBER                  
GLCNTRL  DS    CL(GLVXLENQ)        GLOBBER CONTROL ELEM SAVE AREA               
LINES    EQU   15                  # OF DISPLAY LINES ON SCREEN                 
LWORK    EQU   *-DMWORK                                                         
*                                                                               
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP          PAYING REP                                   
       ++INCLUDE GEGENSPSAL        SPOT SALESPERSON/POINTPERSON RECORDS         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPBRWFFD                                                       
         EJECT                                                                  
* TWA PROGRAM STORAGE                                                           
         ORG   BRWWORK+104                                                      
STAT     DS    X                   PROGRAM STATUS FLAGS                         
BYGLOB   EQU   X'80'               PROGRAM CALLED BY GLOBBER                    
NOTFIRST EQU   X'40'               NOT FIRST CALL TO PROGRAM                    
CONTINUE EQU   X'20'               LIST DISPLAY CONTINUATION?                   
CANCEL   EQU   X'10'               CANCEL HOTKEY SELECTED                       
SWITCH   EQU   X'08'               FILE SWITCHING USED                          
BAD      EQU   X'04'               BROWSE SESSION INVALID FOR RETURN            
*                                                                               
REC      DS    CL3                 RECORD TYPE                                  
RECDESC  DS    CL15                RECORD DESCRIPTION                           
RECROUTE DS    A                   A(RECORD HANDLING ROUTINE)                   
KEYWORD  DS    CL10                KEYWORD ARGUMENT                             
RECLINE  DS    CL(L'BRWREC1)       RECORD DISPLAY LINE BUILD AREA               
LASTKEY  DS    CL40                                                             
GEKEY    DS    CL40                                                             
GEKEYSV  DS    CL40                                                             
TODAY    DS    CL3                 TODAY'S DATE                                 
MALPHAID DS    CL2                 ALPHA ID OF MASTER                           
ALPHAID  DS    CL2                 ALPHA ID FROM GLOBBER                        
OFFICE   DS    CL2                 OFFICE FROM GLOBBER                          
SRCEAGY  DS    CL2                 SOURCE AGY                                   
SRCEUTL  DS    X                   SOURCE FILE                                  
ORIGUTL  DS    X                   ORIGINAL FILE                                
CURAGY   DS    CL2                 CURRENT AGY                                  
*                                                                               
MISCFLG1 DS    X                   FILTER FLAG                                  
MF1SPER  EQU   X'80'                 LIST SALESPERSONS                          
MF1PPER  EQU   X'40'                 LIST POINTPERSONS                          
*                                                                               
OPT      DS    X                   OPTIONS FOR OPTION FIELD                     
OPTSPER  EQU   X'80'               SALESPERSON OPTION                           
OPTPPER  EQU   X'40'               POINTPERSON OPTION                           
*                                                                               
OPTIONS  DS    X                   RECORD OPTIONS FROM CALLER PROG              
**GUIDE TO RECORD OPTIONS**********************************************         
*    REP RECORDS                                                      *         
*        - NO OPTIONS                                                 *         
***********************************************************************         
         EJECT                                                                  
LINED    DSECT                                                                  
LINSALN  DS    CL20                SALESPERSON NAME                             
         DS    CL5                                                              
LINOFF   DS    CL2                 OFFICE                                       
         DS    CL9                                                              
LINPP    DS    CL1                 POINT PERSON?                                
         DS    CL1                                                              
LINREP   DS    CL2                 REP                                          
         DS    CL2                                                              
LINSALC  DS    CL3                 SALESPERSON CODE                             
         DSECT                                                                  
       ++INCLUDE FATWA                                                          
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLBRW                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041SPBRW00   03/25/03'                                      
         END                                                                    
