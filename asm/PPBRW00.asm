*          DATA SET PPBRW00    AT LEVEL 072 AS OF 02/07/14                      
*PHASE T41600A                                                                  
*INCLUDE PPGENWLD                                                               
         TITLE 'PPBRW00 - T41600 - PRT RECORD BROWSER/SEARCH FACILITY'          
*********************************************************************           
* PROGRAMMER NOTE: DOCUMENTATION FOR THE USE AND MAINTENANCE OF     *           
* THIS PROGRAM IS PROVIDED IN REBRWTXT                              *           
*********************************************************************           
*********************************************************************           
*                                                                   *           
*        PPBRW00 --- PRT RECORD BROWSER/SEARCH FACILITY             *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* NOV01/96 RHV  IT'S ALIVE!                                         *           
* AUG22/97 RHV  SUPPORT FILE SWITCHING                              *           
* SEP04/97 RHV  SUPPORT REBROWSE                                    *           
*               'BAD' RETURN PROVISION                              *           
*               'CAN' RETURN PROVISION                              *           
* JUN09/99 RHV  PRODUCT RECORD HANDLING                             *           
* JUN01/00 RHV  SECDEF ACCESS LEVEL HANDLING                        *           
* JUN28/00 BU   REMOVE REFERENCE TO GLV1GOTO PER MEL HERTZIG        *           
* AUG10/01 RHV  BUYTMP RECORD HANDLING                              *           
* SEP/01   SMYE MODIFIED FROM REP TO PRINT (STD SPACE,CLT,PRD)      *           
* MAR/03   SMYE ADD ESTIMATE RECORD HANDLING                        *           
* MAR/04   SMYE ADD CUSTOM COLUMN RECORD HANDLING                   *           
* JAN/05   SMYE ADD PRINT REP RECORD HANDLING                       *           
* FEB/05   SMYE ADD PRINT JOB AND Ad-ID RECORD HANDLING             *           
* MAY17/05 SMYE DIE IF NOT CALL TO PRIBRO IN SETUP                  *           
* FEB/14   BPLA JUST CLEAR GLOBBER CALLS THAT ARE NOT TO PRIBRO                 
*               THEY WERE PROBABLY JUST LEFT OVER FROM A            *           
*               PROGRAM THAT SHOULD HAVE CLEARED IT                             
*                                                                   *           
*                ***  END TOMBSTONE  ***                            *           
*********************************************************************           
T41600   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWORK,*T41600*,R9,RR=R7                                          
*                                                                               
         USING GENOLD,RC                                                        
         USING T416FFD,RA                                                       
*                                                                               
         LR    R8,R1               SAVE R1                                      
         BAS   RE,INITL            CLEAR WORK AREA                              
         NI    STAT,X'FF'-NOTFIRST  ??????? SET STAT NOTFIRST OFF ?????         
         LR    R1,R8               RESTORE R1                                   
*                                                                               
         ST    R7,RELO             ROOT PHASE RELO FACTOR                       
*                                                                               
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   GLOBBER,CGLOBBER-COMFACSD(RF)     GET GLOBBER ADDR               
         ST    R1,SYSFAC           SAVE A(FACILITIES)                           
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RE,CGETTXT                                                       
         ST    RE,GETTXT           SAVE A(GETTXT)                               
         DROP  RF                                                               
*                                                                               
***********************************************************************         
* MAIN PROGRAM                                                        *         
***********************************************************************         
*                                                                               
         LA    R5,BRWSEL1H                                                      
         ST    R5,PTRREC           SAVE POINTER TO 1ST SELECT FIELD             
         LA    R5,BUFFER                                                        
         ST    R5,PTRBUF           SAVE POINTER TO START OF BUFFER              
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
         OC    SRCEREP,SRCEREP                                                  
         BNZ   *+6                                                              
         DC    H'0'                MUST PROVIDE SOURCE REP CODE                 
         OI    STAT,SWITCH                                                      
*                                                                               
MAIN020  DS    0H                                                               
         TM    STAT,SWITCH                                                      
         BZ    MAIN025                                                          
         MVC   CURREP,SRCEREP      USE SOURCE REP                               
         BAS   RE,SETFIL           SWITCH FILE                                  
*                                                                               
MAIN025  DS    0H                                                               
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
         BE    MAIN035             NO                                           
         NI    STAT,X'FF'-CONTINUE                                              
         LA    R3,2                                                             
         BAS   RE,CKREC            LOOKUP NEW REC TYPE                          
         BNE   MYERR                                                            
         MVC   REC,8(R2)                                                        
*                                                                               
MAIN035  DS    0H                                                               
         CLC   BRWREC,=C'CCR'      CUSTOM COLUMN RECORD ?                       
         BNE   MAIN037             NO                                           
         MVI   QMED,0              NOT NEEDED FOR CUSTOM COLUMNS                
         LA    R2,BRWKEYH                                                       
         XC    8(9,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'         XMIT                                         
         B     MAIN080             SKIP CLT,PRD,EST LOGIC                       
*                                                                               
MAIN037  LA    R2,BRWKEYH          MEDIA,CLIENT,PRODUCT CODE FIELD              
         LA    R3,1                                                             
         CLI   5(R2),0                                                          
         BE    MYERR               MEDIA IS REQUIRED                            
         CLC   QMED,8(R2)          MED CHANGED?                                 
         BE    MAIN040             NO                                           
         NI    STAT,X'FF'-CONTINUE                                              
         LA    R3,13               INVALID MEDIA                                
         BAS   RE,VALMED           VALIDATE MEDIA                               
         BNE   MYERR                                                            
*                                                                               
MAIN040  DS    0H                                                               
         LA    R2,BRWKEYH                                                       
         CLC   BRWREC,=C'PRD'      PRODUCT REC?                                 
         BE    MAIN044                                                          
         CLC   BRWREC,=C'EST'      ESTIMATE REC?                                
         BE    MAIN044                                                          
         CLC   BRWREC,=C'JOB'      JOB REC?                                     
         BE    MAIN044                                                          
         CLC   BRWREC,=C'ADI'      Ad-ID (JOB) REC?                             
         BE    MAIN044                                                          
         XC    9(8,R2),9(R2)       NO - CLEAR FIELD AFTER MEDIA                 
         MVI   5(R2),1             MEDIA CODE STILL THERE                       
         OI    6(R2),X'80'                                                      
         B     MAIN080                                                          
*                                                                               
MAIN044  DS    0H                                                               
         LA    R3,186              CLT CODE REQUIRED                            
         CLC   BRWREC,=C'PRD'      PRODUCT REC?                                 
         BE    *+8                 YES                                          
         LA    R3,256              BOTH CLT AND PRD CODE REQUIRED               
         OC    9(8,R2),SPACES                                                   
         CLC   9(4,R2),SPACES      ANY CLT CODE ?                               
         BE    MYERR               NO - ERROR                                   
         CLI   12(R2),C','         2-CHARACTER CLIENT CODE ?                    
         BNE   MAIN046             NO                                           
         CLC   10(2,R2),QCLT       MODIFIED?                                    
         BE    MAIN048             NO                                           
MAIN046  CLC   QCLT,10(R2)         MODIFIED?                                    
         BE    MAIN048             NO                                           
         NI    STAT,X'FF'-CONTINUE                                              
         LA    R3,14               INVALID CLIENT                               
         BAS   RE,VALCLT           VALIDATE CLIENT                              
         BNE   MYERR                                                            
MAIN048  CLC   BRWREC,=C'PRD'      PRODUCT REC?                                 
         BNE   MAIN050             NO - MUST BE ESTIMATE, JOB OR AD ID          
         CLI   12(R2),C','         2-CHARACTER CLIENT CODE ?                    
         BNE   MAIN049             NO                                           
         XC    12(5,R2),12(R2)     CLEAR FIELD AFTER CLIENT                     
         MVI   5(R2),4             MEDIA, CLT STILL THERE                       
         OI    6(R2),X'80'                                                      
         B     MAIN080                                                          
MAIN049  DS    0H                  3-CHARACTER CLIENT CODE                      
         XC    13(4,R2),13(R2)     CLEAR FIELD AFTER CLIENT                     
         MVI   5(R2),5             MEDIA, CLT STILL THERE                       
         OI    6(R2),X'80'                                                      
         B     MAIN080                                                          
*                                                                               
MAIN050  DS    0H                  ESTIMATE, JOB OR AD ID SEARCH                
         LA    R3,256              BOTH CLT AND PRD CODE REQUIRED               
         CLC   13(3,R2),SPACES     ANY PRD CODE ?                               
         BE    MYERR               NO - ERROR                                   
         CLI   12(R2),C','         2-CHARACTER CLIENT CODE ?                    
         BNE   MAIN052             NO                                           
         CLC   13(3,R2),QPRD       MODIFIED?                                    
         BE    MAIN080             NO                                           
         B     MAIN054                                                          
MAIN052  CLC   QPRD,14(R2)         MODIFIED?                                    
         BE    MAIN080             NO                                           
MAIN054  NI    STAT,X'FF'-CONTINUE                                              
         LA    R3,15               INVALID PRODUCT                              
         BAS   RE,VALPRD           VALIDATE PRODUCT                             
         BNE   MYERR                                                            
*                                                                               
MAIN080  DS    0H                                                               
         XC    BRWMSG1,BRWMSG1                                                  
         TM    STAT,BYGLOB         HERE FROM GLOBBER?                           
         BZ    *+14                                                             
         MVC   BRWMSG1(L'MSGDISP),MSGDISP  YES - OFFER SELECT TO USERS          
         B     *+10                                                             
         MVC   BRWMSG1(L'MSGDIS2),MSGDIS2  NO - DON'T OFFER SELECT              
         OI    BRWMSG1H+6,X'80'                                                 
*                                                                               
         ICM   RF,15,RECROUTE      RECORD HANDLING ROUTINE                      
         A     RF,RELO                                                          
         BASR  RE,RF               GO HANDLE RECORD TYPE                        
*                                                                               
         TM    STAT,CONTINUE       WILL DISPLAY BE CONTINUING?                  
         BZ    MAIN084                                                          
         MVC   BRWMSG2(L'MSGCNT),MSGCNT YES - TELL USER TO HIT ENTER            
         B     MAIN200             AND GO WRAP IT UP AND LEAVE                  
*                                                                               
MAIN084  DS    0H                                                               
         CLI   LINENUM,1           ANY RECORDS DISPLAYED?                       
         BNE   MAIN100             YES - DON'T WORRY                            
         TM    STAT,BYGLOB         HERE BY GLOBBER?                             
         BZ    *+12                NO  - SKIP                                   
         TM    STAT,NOTFIRST       YES - ON FIRST TRANSACTION?                  
         BZ    MAIN090                   YES - RETURN TO CALLER                 
         MVC   BRWMSG2(L'MSGNONE),MSGNONE                                       
         B     MAIN200                                                          
*                                                                               
MAIN090  DS    0H                                                               
         BAS   RE,CKBAD            CHECK IF OK TO DO RETURN                     
         TM    STAT,BAD                                                         
         BO    RETURN                                                           
*                                  CREATE AN EMPTY GLOBBER BROWSE ELEM          
         LA    R5,GLOBELEM         (DO THIS WHEN HERE FROM GLOBBER AND          
         USING GLBRWKW,R5          NO RECORDS WERE FOUND)                       
         XC    GLBRWKW,GLBRWKW                                                  
         GOTO1 GLOBBER,DMCB,=C'PUTD',(R5),GLBRWLNQ,GLRBRWSE                     
         CLI   8(R1),0                                                          
         BE    RETURN                                                           
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
MAIN100  DS    0H                                                               
         MVC   BRWMSG2(L'MSGEOR),MSGEOR  ALL RECORDS DISPLAYED MSG              
*                                                                               
*                                                                               
MAIN200  DS    0H                                                               
         OI    BRWMSG2H+6,X'80'    XMIT MSG2 FIELD                              
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
         MVC   GLVXFRSY,=C'PRI'                                                 
         MVC   GLVXFRPR,=C'BRO'                                                 
         MVC   GLVXTOSY(6),WORK    ORIGINAL 'FROM' SYS & PGM                    
***>>>   MVI   GLVXFLG1,GLV1RETN+GLV1SEPS+GLV1GOTO                              
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
* VALMED - VALIDATE THE MEDIA                                         *         
***********************************************************************         
VALMED   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PAGYRECD,R4                                                      
         MVC   PAGYKAGY,AGYALPHA                                                
         MVC   PAGYKMED,BRWKEY                                                  
         MVI   PAGYKRCD,X'01'                                                   
         BAS   RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BNE   NOTEQUAL                                                         
         MVC   QMED,BRWKEY                                                      
         B     EQUAL                                                            
*                                                                               
***********************************************************************         
* VALCLT - VALIDATE THE CLIENT                                        *         
***********************************************************************         
VALCLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
         MVC   PCLTKAGY,AGYALPHA                                                
         MVC   PCLTKMED,BRWKEY                                                  
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,BRWKEY+2                                                
         CLI   BRWKEY+4,C','       2-CHAR. CLIENT ?                             
         BNE   *+8                 NO                                           
         MVI   PCLTKCLT+2,C' '                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(7),KEYSAVE                                                   
         BNE   NOTEQUAL                                                         
         MVC   QCLT,PCLTKCLT                                                    
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         MVC   QCLNAM,PCLTNAME                                                  
         B     EQUAL                                                            
*                                                                               
***********************************************************************         
* VALPRD - VALIDATE THE PRODUCT                                       *         
***********************************************************************         
VALPRD   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PPRDRECD,R4                                                      
         MVC   PPRDKAGY,AGYALPHA                                                
         MVC   PPRDKMED,BRWKEY                                                  
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,BRWKEY+2                                                
         CLI   BRWKEY+4,C','       2-CHAR. CLIENT ?                             
         BNE   VALPRD4             NO                                           
         MVI   PPRDKCLT+2,C' '                                                  
         MVC   PPRDKPRD,BRWKEY+5                                                
         B     *+10                                                             
VALPRD4  MVC   PPRDKPRD,BRWKEY+6                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BNE   NOTEQUAL                                                         
         MVC   QPRD,PPRDKPRD                                                    
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         MVC   QPRNAM,PPRDNAME                                                  
         B     EQUAL                                                            
*                                                                               
***********************************************************************         
* CKREC - LOOKUP RECORD TYPE IN TABLE                                 *         
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
*                                                                               
***********************************************************************         
* DISPREC - DISPLAY RECORD & BUFFER KEYFIELD                          *         
***********************************************************************         
DISPREC  NTR1                                                                   
         CLI   LINENUM,LINES                                                    
         BNH   DR050                                                            
         OI    STAT,CONTINUE                                                    
         MVC   LASTKEY,KEY                                                      
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
         L     R4,PTRBUF                                                        
         MVC   0(L'RECBUF,R4),RECBUF                                            
         MVC   8(L'RECLINE,R3),RECLINE                                          
         OI    6(R3),X'80'                                                      
         LA    R2,BRWSEL2H-BRWSEL1H(R2)                                         
         LA    R4,L'BUFFER(R4)                                                  
         ST    R2,PTRREC                                                        
         ST    R4,PTRBUF                                                        
         ZIC   R3,LINENUM                                                       
         LA    R3,1(R3)                                                         
         STC   R3,LINENUM                                                       
         B     EQUAL                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* STDREC - PROCESS STD RECORDS (STANDARD SPACE DESCRIPTION)           *         
***********************************************************************         
STDREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PSPCDESD,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    STD010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     STD020                                                           
STD010   DS    0H                                                               
         MVI   PSPDRCOD,PSPDRECQ   RECORD CODE (X'5A' - DIR ONLY)               
         MVI   COMPLEN,4           DEFAULT KEY COMPARE LENGTH = 4               
*                                         AGY(2)/MED/RC                         
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    STD020              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(17,PSPDDESC),X'80',RR=Y          
         ZIC   RF,4(R1)            # OF CHAR TO COMPARE FROM PPGENWLD           
         ZIC   RE,COMPLEN          # DEFAULT COMPARE LENGTH                     
         AR    RF,RE                                                            
         STC   RF,COMPLEN          TOTAL KEY COMPARE LENGTH                     
STD020   DS    0H                                                               
         MVC   PSPDAGY,AGYALPHA                                                 
         MVC   PSPDMED,QMED                                                     
         BAS   RE,HIGH                                                          
         B     STD040                                                           
STD030   DS    0H                                                               
         BAS   RE,SEQ                                                           
STD040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    STD050              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(17,PSPDDESC),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    STD030                                                           
STD050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   STD060                                                           
         MVC   BRWHEAD(21),=C'STD Space Description'                            
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(21),DASHES                                                
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
STD060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'PSPDDESC),PSPDDESC                                     
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'PSPDDESC),PSPDDESC                                      
         BAS   RE,DISPREC                                                       
         BE    STD030                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CLTREC - PROCESS CLT RECORDS (CLIENT NAMES)                         *         
***********************************************************************         
CLTREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    CLT010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     CLT020                                                           
CLT010   DS    0H                                                               
         MVC   PCLTKAGY,AGYALPHA                                                
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,X'02'      RECORD CODE                                  
         MVI   COMPLEN,4           DEFAULT KEY COMPARE LENGTH = 4               
*                                         AGY(2)/MED/RC                         
CLT020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     CLT040                                                           
CLT030   DS    0H                                                               
         BAS   RE,SEQ                                                           
CLT040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    CLT050              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(20,PCLTNAME),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    CLT030                                                           
CLT050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   CLT060                                                           
         MVC   BRWHEAD(11),=C'Client Name'                                      
         MVC   BRWHEAD+24(4),=C'Code'                                           
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(11),DASHES                                                
         MVC   BRWUL2+24(4),DASHES                                              
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
CLT060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'PCLTNAME),PCLTNAME                                     
         MVC   RECLINE+24(L'PCLTKCLT),PCLTKCLT                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'PCLTKCLT),PCLTKCLT                                      
         MVC   RECBUF+L'GLBRWKW(L'PCLTNAME),PCLTNAME                            
         BAS   RE,DISPREC                                                       
         BE    CLT030                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRDREC - PROCESS PRD RECORDS (PRODUCT NAMES)                        *         
***********************************************************************         
PRDREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PPRDRECD,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    PRD010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     PRD020                                                           
PRD010   DS    0H                                                               
         MVC   PPRDKAGY,AGYALPHA                                                
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,X'06'      RECORD CODE                                  
         MVC   PPRDKCLT,QCLT                                                    
         MVI   COMPLEN,7           DEFAULT KEY COMPARE LENGTH = 7               
*                                         AGY(2)/MED/RC/CLT(3)                  
PRD020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     PRD040                                                           
PRD030   DS    0H                                                               
         BAS   RE,SEQ                                                           
PRD040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    PRD050              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(20,PPRDNAME),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    PRD030                                                           
PRD050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   PRD060                                                           
         MVC   BRWHEAD(12),=C'Product Name'                                     
         MVC   BRWHEAD+24(4),=C'Code'                                           
         MVC   BRWHEAD+36(13),=C'Client  Name:'                                 
         MVC   BRWHEAD+50(L'QCLNAM),QCLNAM                                      
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(12),DASHES                                                
         MVC   BRWUL2+24(4),DASHES                                              
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
PRD060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'PPRDNAME),PPRDNAME                                     
         MVC   RECLINE+24(L'PPRDKPRD),PPRDKPRD                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'PPRDKPRD),PPRDKPRD                                      
         MVC   RECBUF+L'GLBRWKW(L'PPRDNAME),PPRDNAME                            
         BAS   RE,DISPREC                                                       
         BE    PRD030                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ESTREC - PROCESS EST RECORDS (ESTIMATE NAMES)                       *         
***********************************************************************         
ESTREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PESTRECD,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    EST010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     EST020                                                           
EST010   DS    0H                                                               
         MVC   PESTKAGY,AGYALPHA                                                
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'      RECORD CODE                                  
         MVC   PESTKCLT,QCLT                                                    
         MVC   PESTKPRD,QPRD                                                    
         MVI   COMPLEN,10          DEFAULT KEY COMPARE LENGTH = 10              
*                                     AGY(2)/MED/RC/CLT(3)/PRD(3)               
EST020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     EST040                                                           
EST030   DS    0H                                                               
         BAS   RE,SEQ                                                           
EST040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    EST050              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(20,PESTNAME),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    EST030                                                           
EST050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   EST060                                                           
         MVC   BRWHEAD(13),=C'Estimate Name'                                    
         MVC   BRWHEAD+24(4),=C'Code'                                           
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWHEAD+36(13),=C'Client  Name:'                                 
         MVC   BRWHEAD+50(L'QCLNAM),QCLNAM                                      
         MVC   BRWUL2(13),DASHES                                                
         MVC   BRWUL2+24(4),DASHES                                              
         MVC   BRWUL2+36(13),=C'Product Name:'                                  
         MVC   BRWUL2+50(L'QPRNAM),QPRNAM                                       
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
EST060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'PESTNAME),PESTNAME                                     
         EDIT  (B2,PESTKEST),(3,RECLINE+24),0,ALIGN=LEFT                        
         XC    RECBUF,RECBUF                                                    
         EDIT  (B2,PESTKEST),(3,RECBUF),0,ALIGN=LEFT                            
         MVC   RECBUF+L'GLBRWKW(L'PESTNAME),PESTNAME                            
         BAS   RE,DISPREC                                                       
         BE    EST030                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* JOBREC - PROCESS JOB RECORDS (CAPTION LINE 1)                       *         
***********************************************************************         
JOBREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PJOBRECD,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    JOB010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     JOB020                                                           
JOB010   DS    0H                                                               
         MVC   PJOBKAGY,AGYALPHA                                                
         MVC   PJOBKMED,QMED                                                    
         MVI   PJOBKRCD,X'15'      RECORD CODE                                  
         MVC   PJOBKCLT,QCLT                                                    
         MVC   PJOBKPRD,QPRD                                                    
         MVI   COMPLEN,10          DEFAULT KEY COMPARE LENGTH = 10              
*                                     AGY(2)/MED/RC/CLT(3)/PRD(3)               
JOB020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     JOB040                                                           
JOB030   DS    0H                                                               
         BAS   RE,SEQ                                                           
JOB040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         CLC   KEY+16(6),=6X'00'   JOB HEADER REC ?                             
         BNE   JOB030              NO - SKIP                                    
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    JOB050              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(25,PJOBCAP1),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    JOB030                                                           
JOB050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   JOB060                                                           
         MVC   BRWHEAD+26(04),=C'CODE'                                          
         MVC   BRWHEAD+00(14),=C'CAPTION LINE 1'                                
         MVC   BRWHEAD+36(05),=C'Ad-ID'                                         
         MVC   BRWHEAD+47(07),=C'Client:'                                       
         MVC   BRWHEAD+54(L'QCLNAM),QCLNAM                                      
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2+26(04),DASHES                                             
         MVC   BRWUL2+00(14),DASHES                                             
         MVC   BRWUL2+34(05),DASHES                                             
         MVC   BRWUL2+46(08),=C'Product:'                                       
         MVC   BRWUL2+54(L'QPRNAM),QPRNAM                                       
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
JOB060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE+26(L'PJOBKJOB),PJOBKJOB                                  
         MVC   RECLINE+00(L'PJOBCAP1),PJOBCAP1                                  
         MVC   RECLINE+34(L'PJOBADID),PJOBADID                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'PJOBKJOB),PJOBKJOB                                      
         MVC   RECBUF+L'GLBRWKW(L'PJOBCAP1),PJOBCAP1                            
         BAS   RE,DISPREC                                                       
         BE    JOB030                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ADIREC - PROCESS Ad-ID RECORDS (CAPTION - LINE 1)                   *         
***********************************************************************         
ADIREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PADIRECD,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    ADI010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     ADI020                                                           
ADI010   DS    0H                                                               
         MVC   PADIKAGY,AGYALPHA                                                
         MVC   PADIKMED,QMED                                                    
         MVI   PADIKRCD,PADIKRCQ   RECORD CODE (X'C1')                          
         MVC   PADIKCLT,QCLT                                                    
         MVC   PADIKPRD,QPRD                                                    
         MVI   COMPLEN,10          DEFAULT KEY COMPARE LENGTH = 10              
*                                     AGY(2)/MED/RC/CLT(3)/PRD(3)               
         DROP  R4                                                               
ADI020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     ADI040                                                           
ADI030   DS    0H                                                               
         BAS   RE,SEQ                                                           
ADI040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         USING PJOBRECD,R4                                                      
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    ADI050              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(25,PJOBCAP1),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    ADI030                                                           
ADI050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   ADI060                                                           
         MVC   BRWHEAD+26(5),=C'Ad-ID'                                          
         MVC   BRWHEAD+40(4),=C'CODE'                                           
         MVC   BRWHEAD+00(14),=C'Caption Line 1'                                
         MVC   BRWHEAD+47(07),=C'Client:'                                       
         MVC   BRWHEAD+54(L'QCLNAM),QCLNAM                                      
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2+26(5),DASHES                                              
         MVC   BRWUL2+40(4),DASHES                                              
         MVC   BRWUL2+00(14),DASHES                                             
         MVC   BRWUL2+46(08),=C'Product:'                                       
         MVC   BRWUL2+54(L'QPRNAM),QPRNAM                                       
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
ADI060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE+26(L'PJOBADID),PJOBADID                                  
         MVC   RECLINE+40(L'PJOBKJOB),PJOBKJOB                                  
         MVC   RECLINE+00(L'PJOBCAP1),PJOBCAP1                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'PADIKADI),KEY+10     (Ad-ID IN PASSIVE KEY)             
         MVC   RECBUF+L'GLBRWKW(L'PJOBCAP1),PJOBCAP1                            
         BAS   RE,DISPREC                                                       
         BE    ADI030                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CCCREC - PROCESS CUSTOM COLUMN RECORDS (CODES,TYPE,DESCRIPTION)     *         
***********************************************************************         
CCCREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCOLRECD,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    CCC010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     CCC020                                                           
CCC010   DS    0H                                                               
         MVC   PCOLKAGY,AGYALPHA                                                
         MVI   PCOLKMED,C'A'                                                    
         MVI   PCOLKRCD,X'61'      RECORD CODE                                  
         MVI   COMPLEN,4           DEFAULT KEY COMPARE LENGTH = 4               
*                                         AGY(2)/MED/RC                         
CCC020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     CCC040                                                           
CCC030   DS    0H                                                               
         BAS   RE,SEQ                                                           
CCC040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    CCC050              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(25,PCOLDESC),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    CCC030                                                           
CCC050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   CCC060                                                           
         MVC   BRWHEAD(11),=C'Description'                                      
         MVC   BRWHEAD+28(4),=C'Type'                                           
         MVC   BRWHEAD+35(11),=C'Valid Media'                                   
         MVC   BRWHEAD+49(11),=C'Column Code'                                   
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(11),DASHES                                                
         MVC   BRWUL2+28(4),DASHES                                              
         MVC   BRWUL2+35(11),DASHES                                             
         MVC   BRWUL2+49(11),DASHES                                             
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
CCC060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE+49(L'PCOLKCOD),PCOLKCOD                                  
         MVC   RECLINE+29(L'PCOLTYP),PCOLTYP                                    
*                                                                               
         LA    R1,RECLINE+35       DISPLAY "ALLOWED" MEDIA                      
         CLI   PCOLMED,X'FC'                                                    
         BNE   DM90                                                             
         MVC   0(3,R1),=C'ALL'                                                  
         B     DM90X               DONE WITH MEDIA                              
DM90     DS    0H                                                               
         TM    PCOLMED,X'80'                                                    
         BNO   *+14                                                             
         MVC   0(2,R1),=C'I,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    PCOLMED,X'40'                                                    
         BNO   *+14                                                             
         MVC   0(2,R1),=C'M,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    PCOLMED,X'20'                                                    
         BNO   *+14                                                             
         MVC   0(2,R1),=C'N,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    PCOLMED,X'10'                                                    
         BNO   *+14                                                             
         MVC   0(2,R1),=C'O,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    PCOLMED,X'08'                                                    
         BNO   *+14                                                             
         MVC   0(2,R1),=C'S,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         TM    PCOLMED,X'04'                                                    
         BNO   *+14                                                             
         MVC   0(2,R1),=C'T,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
         AHI   R1,-1                                                            
         MVI   0(R1),C' '         CLEAR LAST COMMA                              
DM90X    DS    0H                                                               
*                                                                               
         MVC   RECLINE(L'PCOLDESC),PCOLDESC                                     
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'PCOLKCOD),PCOLKCOD                                      
         MVC   RECBUF+L'GLBRWKW(L'PCOLDESC),PCOLDESC                            
         BAS   RE,DISPREC                                                       
         BE    CCC030                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REPREC - PROCESS REP RECORDS (REP NAMES)                            *         
***********************************************************************         
REPREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PREPRECD,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    REP010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     REP020                                                           
REP010   DS    0H                                                               
         MVC   PREPKAGY,AGYALPHA                                                
         MVC   PREPKMED,QMED                                                    
         MVI   PREPKRCD,X'11'      RECORD CODE                                  
         MVI   COMPLEN,4           DEFAULT KEY COMPARE LENGTH = 4               
*                                         AGY(2)/MED/RC                         
REP020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     REP040                                                           
REP030   DS    0H                                                               
         BAS   RE,SEQ                                                           
REP040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         L     R4,AREC                                                          
         BAS   RE,GETREC                                                        
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    REP050              NO                                           
         GOTO1 =V(PPGENWLD),DMCB,(20,KEYWORD),(30,PREPNAME),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    REP030                                                           
REP050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   REP060                                                           
         MVC   BRWHEAD(08),=C'Rep Name'                                         
         MVC   BRWHEAD+34(4),=C'Code'                                           
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(08),DASHES                                                
         MVC   BRWUL2+34(4),DASHES                                              
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
REP060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'PREPNAME),PREPNAME                                     
         MVC   RECLINE+34(L'PREPKREP),PREPKREP                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'PREPKREP),PREPKREP                                      
         MVC   RECBUF+L'GLBRWKW(L'PREPNAME),PREPNAME                            
         BAS   RE,DISPREC                                                       
         BE    REP030                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
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
         XC    BRWMSG2,BRWMSG2     CLEAR MESSAGE 2                              
         OI    BRWMSG2H+6,X'80'                                                 
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
         LA    R4,BUFFER                                                        
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
         LA    R4,L'BUFFER(R4)     NEXT BUFFER ENTRY                            
         LA    R2,BRWSEL2H-BRWSEL1H(R2) NEXT SELECT FIELD                       
         BCT   R5,CKSEL010         LOOP                                         
         B     NOTEQUAL            YES - EXIT                                   
CKSEL030 DS    0H                                                               
         BAS   RE,CKBAD            CHECK IF OK TO DO RETURN                     
         TM    STAT,BAD                                                         
         BO    EQUAL                                                            
         LA    R5,GLOBELEM                                                      
         USING GLBRWKW,R5                                                       
         MVC   GLBRWKW,0(R4)       MOVE BUFFER ENTRY INTO GLOBBER ELEM          
         MVC   GLBRWRD,L'GLBRWKW(R4)                                            
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
         MVI   BADCALL,C'N'                                                     
*                                                                               
*NOP*    MVI   OPTIONS,0           DEFAULT NO OPTIONS                           
*                                                                               
         MVC   CURREP,AGYALPHA     DEFAULT CURRENT AGENCY                       
*                                                                               
*****    XC    KEY,KEY                                                          
*****    LA    R4,KEY                                                           
*****    USING PAGYKEY,R4                                                       
*****    MVI   PAGYKRCD,1                                                       
*****    MVC   PAGYKAGY,CURREP                                                  
*****    BAS   RE,HIGH                                                          
*****    CLC   KEY(25),KEYSAVE                                                  
*****    BE    *+6                                                              
*****    DC    H'0'                                                             
*****    GOTO1 GETREC,DMCB,IOAREA                                               
*****    LA    R4,IOAREA                                                        
*****    MVC   MASTREP,RREPMAST                                                 
*****    DROP  R4                                                               
*****    OC    MASTREP,SPACES                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,TODAY)                                     
*                                                                               
         LA    R4,GLCNTRL                                                       
         USING GLVXFRSY,R4                                                      
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETD',(R4),GLVXLENQ,GLVXCODQ                     
         TM    8(R1),GLEGNF        HAVE GLOBBER CONTROL ELEM?                   
         BO    XIT                 GET OUT OF HERE                              
         CLI   8(R1),0             OTHER GLOBBER ERRORS?                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GLVXTOSY(6),=C'PRIBRO'                                           
         BE    SETUP5                                                           
         MVI   BADCALL,C'Y'                                                     
*******  DC    H'0'                NOT A PRINT BROWSE CALL                      
*******                            NO LONGER DIE - JUST CLEAR                   
*                                                                               
SETUP5   GOTO1 GLOBBER,DMCB,=C'DELE',0,GLVXLENQ,GLVXCODQ                        
         CLI   8(R1),0             GLOBBER ERRORS?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         CLI   BADCALL,C'Y'        IF NOT HERE WITH A BRO CALL                  
         BNE   SETUP8                                                           
         B     XIT          JUST EXIT - LIKE NON-GLOBBER CALL                   
*                                                                               
SETUP8   OI    STAT,BYGLOB         SET CALLED BY GLOBBER FLAG                   
*                                                                               
SETUP10  LA    R4,GLOBELEM                                                      
         USING GLBRWKW,R4                                                       
*                                                                               
*                                  LOOK FOR BROWSE GLOBBER ELEM                 
         GOTO1 GLOBBER,DMCB,=C'GETD',(R4),GLBRWLNQ,GLRBRWSE                     
         CLI   8(R1),0             MUST HAVE GLOBBER BROWSE ELEM                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*NOP*    MVC   OPTIONS,GLBRWFLG    OPTIONS BITS FOR RECORD FORMATS              
*                                                                               
*                                  (THESE ARE FOR FILE SWITCHING ONLY)          
         MVC   SRCEUTL,GLBRWSF     SOURCE FILE                                  
         MVC   ORIGUTL,GLBRWOF     ORIGINAL FILE                                
         MVC   SRCEREP,GLBRWSR     SOURCE REP                                   
*                                                                               
         MVC   PARPRG,GLBRWAPG     PAR PROGRAGM                                 
         MVC   PARREC,GLBRWARC     PAR RECORD TYPE                              
*                                                                               
         LA    R2,BRWRECH          RECORD CODE FIELD IS FOR DDS USE             
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
         LA    R2,BRWKEYH          KEY FIELD IS FOR DDS USE                     
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
         LA    R2,BRWRECLH         RECORD CODE FIELD LABEL                      
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
         LA    R2,BRWKEYLH         KEY FIELD LABEL                              
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    6(R2),X'80'                                                      
         LA    R2,BRWADVH          ADV CODE FIELD IS FOR DDS USE                
         OI    1(R2),X'0C'         ZERO INTENSITY                               
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'                                                      
         LA    R2,BRWPFKLH         PFKEY LABEL                                  
         NI    1(R2),X'FF'-X'0C'   TURN OFF ZERO INTENSITY                      
         OI    6(R2),X'80'                                                      
         LA    R2,BRWRECH          YES - MOVE RECORD TYPE FROM GLOBBER          
         MVC   8(3,R2),GLBRWREC          ELEM INTO RECTYPE FIELD                
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         LA    R2,BRWKEYH          YES - MOVE MEDIA CODE FROM GLOBBER           
         MVC   8(1,R2),GLBRWFLG          ELEM INTO KEY FIELD                    
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         LA    R2,BRWKEYWH         YES - MOVE KEYWORD FROM GLOBBER              
         MVC   8(10,R2),GLBRWKW         ELEM INTO KEYWORD FIELD                 
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         OC    GLBRWCLT,GLBRWCLT                                                
         BZ    SETUP40                                                          
         LA    R2,BRWKEYH          YES - MOVE CLIENT CODE FROM GLOBBER          
         MVC   10(3,R2),GLBRWCLT     ELEM INTO KEY FIELD AFTER MEDIA,           
         MVI   9(R2),C','                                                       
         MVI   5(R2),5             MEDIA(1)+,(1)+CLT CODE(3)                    
         OI    6(R2),X'80'                                                      
         OC    GLBRWPRD,GLBRWPRD                                                
         BZ    SETUP40           MOVE PRODUCT CODE FROM GLOBBER ELEM            
         MVC   14(3,R2),GLBRWPRD     INTO KEY FIELD AFTER MEDIA,CLT,            
         MVI   13(R2),C','                                                      
         MVI   5(R2),9        MED(1)+,(1)+CLT CODE(3)+,(1)+PRD CODE(3)          
         OI    6(R2),X'80'                                                      
SETUP40  MVC   BRWSERV(4),=C'=RE '                                              
         OI    BRWSERVH+6,X'80'                                                 
         DROP  R4                                                               
         B     XIT                                                              
***********************************************************************         
*   ORIGFIL:   SWITCHES TO ORIGINAL REP FILE IF NECESSARY                       
***********************************************************************         
ORIGFIL  NTR1                                                                   
         MVC   CURREP,REPALPHA                                                  
         NI    STAT,X'FF'-SWITCH                                                
         CLC   SRCEUTL,ORIGUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    XIT                 YES - DON'T SWITCH                           
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    XIT                                                              
         DC    H'0'                NO  - ABORT                                  
***********************************************************************         
*   SETFIL:    SWITCHES TO SOURCE REP FILE, IF NECESSARY.                       
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
* RECORD TYPE TABLE                                                   *         
***********************************************************************         
*   BYTE 0-2  = 3 LETTER RECORD CODE                                            
*                                                                               
*   BYTE 3-17 = 15 CHARACTER RECORD TYPE DESCRIPTION TO BE DISPLAYED            
*                                                                               
*   BYTE 18-21= A(ROUTINE TO HANDLE READING OF RECORDS)                         
*                                                                               
RECTAB   DS    0CL22                                                            
         DC    C'STD',CL15'STD SPACE DESC',AL4(STDREC)                          
         DC    C'CLT',CL15'CLIENT',AL4(CLTREC)                                  
         DC    C'PRD',CL15'PRODUCT',AL4(PRDREC)                                 
         DC    C'EST',CL15'ESTIMATE',AL4(ESTREC)                                
         DC    C'CCR',CL15'CUSTOM COLUMN',AL4(CCCREC)                           
         DC    C'REP',CL15'REP RECORD',AL4(REPREC)                              
         DC    C'JOB',CL15'JOB RECORD',AL4(JOBREC)                              
         DC    C'ADI',CL15'Ad-ID RECORD',AL4(ADIREC)                            
         DC    X'FF'                                                            
***********************************************************************         
*                                                                               
NOTEQUAL LTR   RB,RB                                                            
         B     XIT                                                              
EQUAL    CR    RB,RB                                                            
XIT      XIT1                                                                   
MSGCNT   DC    C'More records available. Press ENTER to continue.'              
MSGEOR   DC    C'Records displayed. End of list.'                               
MSGNONE  DC    C'No matching records found.'                                    
MSGCODE  DC    C'PRINTPAK RECORD BROWSER - ENTER RECORD CODE AND KEY'           
MSGDISP  DC    C'PRINTPAK RECORD BROWSER - Select record or change filt+        
               er'                                                              
MSGDIS2  DC    C'PRINTPAK RECORD BROWSER - View records or change filte+        
               r'                                                               
SPACES   DC    CL80' '                                                          
DASHES   DC    80C'-'                                                           
MYERR    DS    0H                                                               
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         OI    STAT,NOTFIRST       NOT ANYMORE                                  
         L     RD,BASERD                                                        
         LTR   R3,R3               IF DATAMGR ERROR, DON'T SET CURPOS           
         BZ    EXIT                                                             
         OI    6(R2),X'40'                                                      
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,33,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
       ++INCLUDE PPGENEROL                                                      
         EJECT                                                                  
       ++INCLUDE PPGENOLD                                                       
         ORG   IOAREA+1008                                                      
         SPACE 2                                                                
BASERB   DS    F                                                                
BASERD   DS    F                                                                
RELO     DS    A                   RELOCATION FACTOR - ROOT PHASE               
RELO1    DS    A                   RELO - FIRST LEVEL OVERLAY                   
SYSFAC   DS    A                   A(SYSTEM FACILITIES)                         
GLOBBER  DS    A                   A(GLOBBER)                                   
GETTXT   DS    A                   A(GETTXT)                                    
PTRBUF   DS    A                   NEXT AVAILABLE BUFFER ENTRY                  
PTRREC   DS    A                   NEXT AVAILABLE REC DISPLAY LINE              
LINENUM  DS    X                   CURRENT DISPLAY LINE NUMBER                  
ELCODE   DS    C                                                                
BADCALL  DS    C                                                                
LWORK    EQU   *-DMWORK                                                         
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PSPCDES                                                        
         EJECT                                                                  
PAGYRECD DSECT                                                                  
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
PCOLRECD DSECT                                                                  
       ++INCLUDE PCOLREC                                                        
         EJECT                                                                  
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
         EJECT                                                                  
PJOBRECD DSECT                                                                  
       ++INCLUDE PJOBREC                                                        
         EJECT                                                                  
****** AD ID DSECT                                                              
       ++INCLUDE PPGENPADID                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE PPBRWFFD                                                       
         EJECT                                                                  
* TWA PROGRAM STORAGE                                                           
         ORG   BRWWORK+100                                                      
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
KEYWORD  DS    CL20                KEYWORD ARGUMENT (WAS 10)                    
COMPLEN  DS    X                   KEY COMPARE LENGTH                           
QMED     DS    CL1                 MEDIA CODE                                   
QCLT     DS    CL3                 CLIENT CODE (FOR PRD REC LOOKUP)             
QPRD     DS    CL3                 PRODUCT CODE (FOR EST REC LOOKUP)            
QCLNAM   DS    CL20                CLIENT NAME (FOR PRD REC DISPLAY)            
QPRNAM   DS    CL20                PRODUCT NAME (FOR EST REC DISPLAY)           
RECLINE  DS    CL(L'BRWREC1)       RECORD DISPLAY LINE BUILD AREA               
RECBUF   DS    CL(L'GLBRWKW+L'GLBRWRD)    BUFFER ENTRY BUILD AREA               
LASTKEY  DS    CL(L'KEY)                                                        
TODAY    DS    CL3                 TODAY'S DATE                                 
SRCEREP  DS    CL2                 SOURCE REP                                   
SRCEUTL  DS    X                   SOURCE FILE                                  
ORIGUTL  DS    X                   ORIGINAL FILE                                
CURREP   DS    CL2                 CURRENT AGENCY                               
MASTREP  DS    CL2                 MASTER REP FIELD FROM RREPREC                
PARPRG   DS    CL3                 PAR ACCESS LEVEL LOOKUP - PROGRAM            
PARREC   DS    CL8                 PAR ACCESS LEVEL LOOKUP - RECORD             
*                                                                               
OPTIONS  DS    X                   RECORD OPTIONS FROM CALLER PROG              
**GUIDE TO RECORD OPTIONS**********************************************         
***********************************************************************         
*    FOR PRINTPAK USE - CONTAINS MEDIA CODE                           *         
***********************************************************************         
*                                                                     *         
*    AGY RECORDS                                                      *         
*        X'80' - RETURN AGY & AGY OFFICE WITHOUT '-' BETWEEN          *         
*        X'40' - AGY ADDRESS ON SCREEN LIST                           *         
*                                                                     *         
*    ADV RECORDS                                                      *         
*        ALL SPARE                                                    *         
*                                                                     *         
*    SAL RECORDS                                                      *         
*        X'80' - DON'T DISPLAY REC IF LEAVE DATE < TODAY              *         
*        REST SPARE                                                   *         
***********************************************************************         
GLOBELEM DS    CL(GLBRWLNQ)        BROWSE ELEMENT FROM GLOBBER                  
GLCNTRL  DS    CL(GLVXLENQ)        GLOBBER CONTROL ELEM SAVE AREA               
LINES    EQU   16                  # OF DISPLAY LINES ON SCREEN                 
BUFFER   DS    (LINES)CL(L'GLBRWKW+L'GLBRWRD)                                   
IO2      DS    XL4096              4K IOAREA                                    
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE FATWA                                                          
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE PPGLBRW                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072PPBRW00   02/07/14'                                      
         END                                                                    
