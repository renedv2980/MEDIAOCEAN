*          DATA SET REBRW00S   AT LEVEL 068 AS OF 11/21/02                      
*PHASE T82200A,*                                                                
*INCLUDE REGENWLD                                                               
         TITLE 'REBRW00 - T82200 - REP RECORD BROWSER/SEARCH FACILITY'          
*********************************************************************           
* PROGRAMMER NOTE: DOCUMENTATION FOR THE USE AND MAINTENANCE OF     *           
* THIS PROGRAM IS PROVIDED IN REBRWTXT                              *           
*********************************************************************           
*********************************************************************           
*                                                                   *           
*        REBRW00 --- REP RECORD BROWSER/SEARCH FACILITY             *           
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
* MAR19/02 RHV  BUY CODE RECORD HANDLING                            *           
* NOV20/02 JRD  STATION RECORD HANDLING                             *           
*               MARKET RECORD HANDLING                              *           
*                                                                   *           
*                                                                   *           
*                ***  END TOMBSTONE  ***                            *           
*********************************************************************           
T82200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWORK,**8220**,R9,RR=R5,CLEAR=YES                                
*                                                                               
         USING GENOLD,RC                                                        
*                                                                               
         ST    R5,RELO             ROOT PHASE RELO FACTOR                       
         ST    R1,SYSFAC           SAVE A(FACILITIES)                           
         L     RE,16(R1)           A(COMFACS)                                   
         ST    RE,ACOMFACS                                                      
         BAS   RE,INITL                                                         
         USING T822FFD,RA                                                       
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RE,CGLOBBER                                                      
         ST    RE,GLOBBER          SAVE A(GLOBBER)                              
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
         BAS   RE,SETFIL           SWTICH FILE                                  
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
         BE    MAIN040                                                          
         NI    STAT,X'FF'-CONTINUE                                              
         LA    R3,630                                                           
         BAS   RE,CKREC            LOOKUP NEW REC TYPE                          
         BNE   MYERR                                                            
         MVC   REC,8(R2)                                                        
*                                                                               
MAIN040  DS    0H                                                               
         CLC   BRWREC,=C'ACC'      PAR ACCESS LEVELS?                           
         BNE   MAIN050             NO                                           
         TM    STAT,BYGLOB         CALLED BY GLOBBER?                           
         BZ    MYERR               NO - VALID ONLY FROM GLOBBER                 
*                                                                               
MAIN050  DS    0H                                                               
         LA    R2,BRWADVH         ADVERTISER CODE FIELD (FOR PRD RECS)          
         CLC   BRWREC,=C'PRD'      PRODUCT REC?                                 
         BE    MAIN051                                                          
         XC    BRWADV,BRWADV       NO - CLEAR ADV FIELD                         
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         B     MAIN052                                                          
MAIN051  DS    0H                                                               
         LA    R3,839              ADV REQUIRED                                 
         OC    BRWADV,SPACES                                                    
         CLC   BRWADV,SPACES      ANY ADV?                                      
         BE    MYERR              NO - ERROR                                    
         CLC   PRDADV,8(R2)       MODIFIED?                                     
         BE    *+14                                                             
         MVC   PRDADV,8(R2)                                                     
         NI    STAT,X'FF'-CONTINUE                                              
*                                                                               
MAIN052  DS    0H                                                               
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
         BZ    MAIN055                                                          
         MVC   BRWMSG2(L'MSGCNT),MSGCNT YES - TELL USER TO HIT ENTER            
         B     MAIN200             AND GO WRAP IT UP AND LEAVE                  
*                                                                               
MAIN055  DS    0H                                                               
         CLI   LINENUM,1           ANY RECORDS DISPLAYED?                       
         BNE   MAIN070             YES - DON'T WORRY                            
         TM    STAT,BYGLOB         HERE BY GLOBBER?                             
         BZ    *+12                NO  - SKIP                                   
         TM    STAT,NOTFIRST       YES - ON FIRST TRANSACTION?                  
         BZ    MAIN060                   YES - RETURN TO CALLER                 
         MVC   BRWMSG2(L'MSGNONE),MSGNONE                                       
         B     MAIN200                                                          
*                                                                               
MAIN060  DS    0H                                                               
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
MAIN070  DS    0H                                                               
         MVC   BRWMSG2(L'MSGEOR),MSGEOR  ALL RECORDS DISPLAYED MSG              
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
         MVC   GLVXFRSY,=C'REP'                                                 
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
***********************************************************************         
* MKTCOD - PROCESS MARKET CODES                                       *         
***********************************************************************         
MKTCOD   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RMKTREC,R4                                                       
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    MKT010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     MKT020                                                           
MKT010   DS    0H                                                               
         MVI   RMKTKTYP,X'AF'      MARKET PASSIVE KEY                           
         MVC   RMKT2REP,CURREP                                                  
         MVI   COMPLEN,1           DEFAULT KEY COMPARE LENGTH = 1               
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    MKT020              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(19,RMKT2NAM),X'80',RR=Y          
         ZIC   RF,4(R1)            # OF CHAR TO COMPARE FROM REGENWLD           
         ZIC   RE,COMPLEN          # DEFAULT COMPARE LENGTH                     
         AR    RF,RE                                                            
         STC   RF,COMPLEN          TOTAL KEY COMPARE LENGTH                     
MKT020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     MKT040                                                           
MKT030   DS    0H                                                               
         MVC   RMKT2REP,CURREP                                                  
         BAS   RE,SEQ                                                           
MKT040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         CLC   CURREP,RMKT2REP                                                  
         BNE   MKT030                                                           
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    MKT050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(19,RMKT2NAM),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    MKT030                                                           
MKT050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   MKT060                                                           
         MVC   BRWHEAD+00(11),=C'MARKET NAME'                                   
         MVC   BRWHEAD+22(04),=C'CODE'                                          
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(20),DASHES                                                
         MVC   BRWUL2+22(04),DASHES                                             
         OI    BRWUL2H+6,X'80'                                                  
*                                                                               
         BAS   RE,SELHEAD                                                       
*                                                                               
MKT060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE+22(L'RMKT2MKT),RMKT2MKT                                  
*                                                                               
**       BAS   RE,GETREC                                                        
**       LA    R6,IOAREA                                                        
**       MVI   ELCODE,X'01'                                                     
**       BAS   RE,GETEL                                                         
**       BNE   MKT070                                                           
*                                                                               
**       USING RMKTELEM,R6                                                      
         MVC   RECLINE+00(L'RMKT2NAM),RMKT2NAM                                  
***      DROP  R6                                                               
*                                                                               
MKT070   DS    0H                                                               
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RMKTKMKT),RMKT2MKT                                      
         MVC   RECBUF+L'GLBRWKW(L'RMKT2MKT),RMKT2MKT                            
*                                                                               
         BAS   RE,DISPREC                                                       
         BE    MKT030                                                           
         B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
* STAREC - PROCESS STA RECORDS (STATION)                              *         
***********************************************************************         
STAREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAREC,R4                                                       
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    STA010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     STA020                                                           
STA010   DS    0H                                                               
         MVI   RSTAKTYP,X'02'      STATION KEY                                  
         MVC   RSTAKREP,CURREP                                                  
         MVI   COMPLEN,1           DEFAULT KEY COMPARE LENGTH = 1               
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    STA020              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(18,RSTAKSTA),X'80',RR=Y          
         ZIC   RF,4(R1)            # OF CHAR TO COMPARE FROM REGENWLD           
         ZIC   RE,COMPLEN          # DEFAULT COMPARE LENGTH                     
         AR    RF,RE                                                            
         STC   RF,COMPLEN          TOTAL KEY COMPARE LENGTH                     
STA020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     STA040                                                           
STA030   DS    0H                                                               
         MVC   RSTAKREP,CURREP                                                  
         BAS   RE,SEQ                                                           
STA040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         CLC   CURREP,RSTAKREP                                                  
         BNE   STA030                                                           
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    STA050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(18,RSTAKSTA),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    STA030                                                           
STA050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   STA060                                                           
         MVC   BRWHEAD(07),=C'STATION'                                          
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(07),DASHES                                                
         OI    BRWUL2H+6,X'80'                                                  
*                                                                               
         BAS   RE,SELHEAD                                                       
*                                                                               
STA060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RSTAKSTA-1),RSTAKSTA                                   
*                                                                               
         LA    RE,RECLINE+(L'RSTAKSTA-1)                                        
         LA    RF,RECLINE                                                       
*                                                                               
         CLI   RSTAKSTA+(L'RSTAKSTA-1),C' '                                     
         BE    STA066                                                           
*                                                                               
STA062   DS    0H                                                               
         CLI   0(RE),C' '                                                       
         BH    STA064                                                           
         BCTR  RE,0                                                             
         CR    RE,RF                                                            
         BH    STA062                                                           
*                                                                               
STA064   DS    0H                                                               
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),RSTAKSTA+(L'RSTAKSTA-1)                                  
         AHI   RE,3                                                             
*                                                                               
STA066   DS    0H                                                               
         SR    RE,RF               LENGTH OF STATION                            
         XC    RECBUF,RECBUF                                                    
*                                                                               
         MVC   RECBUF(L'RSTAKSTA),RSTAKSTA                                      
         MVC   RECBUF+L'GLBRWKW(L'RSTAKSTA),RSTAKSTA                            
*                                                                               
         BAS   RE,DISPREC                                                       
         BE    STA030                                                           
         B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
* AGYREC - PROCESS AGY RECORDS (AGENCY)                               *         
***********************************************************************         
AGYREC   NTR1                                                                   
         TM    STAT,BYGLOB         FROM GLOBBER                                 
         BO    *+8                                                              
         OI    OPTIONS,X'40'       NO - DEFAULT TO SHOW ADDRESS                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RAGY2REC,R4                                                      
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    AGY010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     AGY020                                                           
AGY010   DS    0H                                                               
         MVI   RAG2PTYP,X'BA'      AGENCY2 PASSIVE KEY                          
         MVC   RAG2PREP,CURREP                                                  
         MVI   COMPLEN,1           DEFAULT KEY COMPARE LENGTH = 1               
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    AGY020              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(18,RAG2PNAM),X'80',RR=Y          
         ZIC   RF,4(R1)            # OF CHAR TO COMPARE FROM REGENWLD           
         ZIC   RE,COMPLEN          # DEFAULT COMPARE LENGTH                     
         AR    RF,RE                                                            
         STC   RF,COMPLEN          TOTAL KEY COMPARE LENGTH                     
AGY020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     AGY040                                                           
AGY030   DS    0H                                                               
         MVC   RAG2PREP,CURREP                                                  
         BAS   RE,SEQ                                                           
AGY040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         CLC   CURREP,RAG2PREP                                                  
         BNE   AGY030                                                           
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    AGY050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(18,RAG2PNAM),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    AGY030                                                           
AGY050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   AGY060                                                           
         MVC   BRWHEAD(11),=C'AGENCY NAME'                                      
         MVC   BRWHEAD+20(4),=C'CODE'                                           
         MVC   BRWHEAD+26(3),=C'OFC'                                            
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(11),DASHES                                                
         MVC   BRWUL2+20(4),DASHES                                              
         MVC   BRWUL2+26(3),DASHES                                              
         OI    BRWUL2H+6,X'80'                                                  
         TM    OPTIONS,X'40'       SHOW ADDRESS?                                
         BZ    AGY055                                                           
         MVC   BRWHEAD+31(14),=C'AGENCY ADDRESS'                                
         MVC   BRWUL2+31(14),DASHES                                             
AGY055   DS    0H                                                               
         BAS   RE,SELHEAD                                                       
*                                                                               
AGY060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RAG2PNAM),RAG2PNAM                                     
         MVC   RECLINE+20(L'RAG2PAGY),RAG2PAGY                                  
         MVC   RECLINE+26(L'RAG2PAOF),RAG2PAOF                                  
         TM    OPTIONS,X'40'       SHOW ADDRESS?                                
         BZ    AGY063                                                           
         BAS   RE,GETREC                                                        
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   AGY063                                                           
* PUT OUT CITY, STATE, AND WHATEVER WE HAVE ROOM FOR OF ADDR 1                  
         USING RAGY2AE1,R6                                                      
         OC    RAGY2CTY,SPACES                                                  
         LA    R1,RAGY2CTY                                                      
         LA    R2,L'RAGY2CTY(R1)                                                
         BCTR  R2,0                                                             
         CR    R2,R1                                                            
         BL    *+12                                                             
         CLI   0(R2),C' '                                                       
         BE    *-12                                                             
         LA    R2,1(R2)                                                         
         SR    R2,R1               LEN OF CITY                                  
         AH    R2,=H'4'            + OVERHEAD & STATE                           
         LA    R1,43               ROOM ON LINE                                 
         SR    R1,R2               MAX ROOM FOR ADDR 1                          
         CH    R1,=Y(L'RAGY2AD1)                                                
         BNH   *+8                                                              
         LA    R1,L'RAGY2AD1                                                    
         OC    RAGY2AD1,SPACES                                                  
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   RECLINE+31(0),RAGY2AD1                                           
         LA    R5,RECLINE+31                                                    
         LA    R1,1(R5,R1)                                                      
         BCTR  R1,0                                                             
         CR    R1,R5                                                            
         BL    *+12                                                             
         CLI   0(R1),C' '                                                       
         BE    *-12                                                             
         MVI   1(R1),C','                                                       
         SH    R2,=H'4'                                                         
         LTR   R2,R2                                                            
         BZ    *+6                                                              
         BCTR  R2,0                                                             
         EX    R2,*+4                                                           
         MVC   2(0,R1),RAGY2CTY                                                 
         LA    R1,3(R2,R1)                                                      
         MVI   0(R1),C','                                                       
         MVC   1(2,R1),RAGY2STE                                                 
         DROP  R6                                                               
AGY063   DS    0H                                                               
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RAG2PAGY),RAG2PAGY                                      
         MVC   RECBUF+L'GLBRWKW(L'RAG2PNAM),RAG2PNAM                            
         OC    RAG2PAOF,SPACES                                                  
         CLC   RAG2PAOF,SPACES                                                  
         BE    AGY070                                                           
         TM    OPTIONS,X'80'       RETURN AGY&OFC WITHOUT '-'                   
         BZ    AGY065              NO - DO IT NORMAL WAY                        
         MVC   RECBUF+4(2),RAG2PAOF                                             
         B     AGY070                                                           
AGY065   DS    0H                                                               
         LA    R5,RECBUF                                                        
         CLI   0(R5),0                                                          
         BE    *+20                                                             
         CLI   0(R5),C' '                                                       
         BE    *+12                                                             
         LA    R5,1(R5)                                                         
         B     *-20                                                             
         MVI   0(R5),C'-'                                                       
         MVC   1(2,R5),RAG2PAOF                                                 
AGY070   DS    0H                                                               
         BAS   RE,DISPREC                                                       
         BE    AGY030                                                           
         B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
* ADVREC - PROCESS ADV RECORDS (ADVERTISER)                           *         
***********************************************************************         
ADVREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RADVREC,R4                                                       
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    ADV010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     ADV020                                                           
ADV010   DS    0H                                                               
         MVI   RADVPTYP,X'88'      AGENCY PASSIVE KEY                           
         MVI   COMPLEN,1           DEFAULT KEY COMPARE LENGTH = 1               
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    ADV020              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(20,RADVPNAM),X'80',RR=Y          
         ZIC   RF,4(R1)            # OF CHAR TO COMPARE FROM REGENWLD           
         ZIC   RE,COMPLEN          # DEFAULT COMPARE LENGTH                     
         AR    RF,RE                                                            
         STC   RF,COMPLEN          TOTAL KEY COMPARE LENGTH                     
ADV020   DS    0H                                                               
         MVC   RADVPREP,CURREP                                                  
         BAS   RE,HIGH                                                          
         B     ADV040                                                           
ADV030   DS    0H                                                               
         MVC   RADVPREP,CURREP                                                  
         BAS   RE,SEQ                                                           
ADV040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         CLC   CURREP,RADVPREP                                                  
         BNE   ADV030                                                           
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    ADV050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(20,RADVPNAM),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    ADV030                                                           
ADV050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   ADV060                                                           
         MVC   BRWHEAD(15),=C'ADVERTISER NAME'                                  
         MVC   BRWHEAD+22(4),=C'CODE'                                           
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(15),DASHES                                                
         MVC   BRWUL2+22(4),DASHES                                              
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
ADV060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RADVPNAM),RADVPNAM                                     
         MVC   RECLINE+22(L'RADVPADV),RADVPADV                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RADVPADV),RADVPADV                                      
         MVC   RECBUF+L'GLBRWKW(L'RADVPNAM),RADVPNAM                            
         BAS   RE,DISPREC                                                       
         BE    ADV030                                                           
         B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
* PRDREC - PROCESS PRD RECORDS (PRODUCT)                              *         
***********************************************************************         
PRDREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RPRDREC,R4                                                       
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    PRD010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     PRD020                                                           
PRD010   DS    0H                                                               
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,PRDADV                                                  
         MVI   COMPLEN,22          DEFAULT KEY COMPARE LENGTH = 3               
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
         CLC   CURREP,KEY+25       MATCH REP CODE?                              
         BNE   PRD030                                                           
         DROP  R4                                                               
         BAS   RE,GETREC                                                        
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    PRD050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(20,RPRDNAME),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    PRD030                                                           
PRD050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   PRD060                                                           
         MVC   BRWHEAD(12),=C'PRODUCT NAME'                                     
         MVC   BRWHEAD+22(4),=C'CODE'                                           
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(12),DASHES                                                
         MVC   BRWUL2+22(4),DASHES                                              
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
PRD060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RPRDNAME),RPRDNAME                                     
         MVC   RECLINE+22(L'RPRDKPRD),RPRDKPRD                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RPRDKPRD),RPRDKPRD                                      
         MVC   RECBUF+L'GLBRWKW(L'RPRDNAME),RPRDNAME                            
         BAS   RE,DISPREC                                                       
         BE    PRD030                                                           
         B     XIT                                                              
***********************************************************************         
* BUYTMP - PROCESS TMP RECORDS (BUY TEMPLATE)                         *         
***********************************************************************         
BUYTMP   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RTMPREC,R4                                                       
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    TMP010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     TMP020                                                           
TMP010   DS    0H                                                               
         MVC   RTMPKTYP,=X'1507'                                                
         MVC   RTMPKREP,CURREP                                                  
         MVI   COMPLEN,14          DEFAULT KEY COMPARE LENGTH = 14              
TMP020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     TMP040                                                           
TMP030   DS    0H                                                               
         BAS   RE,SEQ                                                           
TMP040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         CLI   KEY+26,0                                                         
         BNE   TMP030                                                           
         DROP  R4                                                               
         BAS   RE,GETREC                                                        
         TM    RTMPHFLG,X'80'                                                   
         BO    TMP030              INACTIVE                                     
         DS    0H                                                               
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    TMP050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(12,RTMPKTMP),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    TMP030                                                           
TMP050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   TMP060                                                           
         MVC   BRWHEAD(12),=CL12'TEMPLATE'                                      
         MVC   BRWHEAD+14(4),=CL34'DESCRIPTION'                                 
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(12),DASHES                                                
         MVC   BRWUL2+14(34),DASHES                                             
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
TMP060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RTMPKTMP),RTMPKTMP                                     
         MVC   RECLINE+14(L'RTMPHDSC),RTMPHDSC                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(12),RTMPKTMP                                              
         MVC   RECBUF(L'RTMPKTMP),RTMPKTMP                                      
         BAS   RE,DISPREC                                                       
         BE    TMP030                                                           
         B     XIT                                                              
***********************************************************************         
* BUYCOD - PROCESS BUY CODE RECORDS                                   *         
***********************************************************************         
BUYCOD   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSPBREC,R4                                                       
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    BCD010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     BCD020                                                           
BCD010   DS    0H                                                               
         MVI   RSPBKTYP,X'4B'                                                   
         MVC   RSPBKREP,CURREP                                                  
         MVI   RSPBKTCD,X'01'                                                   
         MVI   COMPLEN,24          DEFAULT KEY COMPARE LENGTH = 24              
BCD020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     BCD040                                                           
BCD030   DS    0H                                                               
         BAS   RE,SEQ                                                           
BCD040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         DROP  R4                                                               
         BAS   RE,GETREC                                                        
         DS    0H                                                               
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    BCD050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(20,RSPBDESC),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    BCD030                                                           
BCD050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   BCD060                                                           
         MVC   BRWHEAD(12),=CL12'BUY CODE'                                      
         MVC   BRWHEAD+14(4),=CL20'DESCRIPTION'                                 
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(12),DASHES                                                
         MVC   BRWUL2+14(20),DASHES                                             
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
BCD060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RSPBKCOD),RSPBKCOD                                     
         MVC   RECLINE+14(L'RSPBDESC),RSPBDESC                                  
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RSPBKCOD),RSPBKCOD                                      
         BAS   RE,DISPREC                                                       
         BE    BCD030                                                           
         B     XIT                                                              
***********************************************************************         
* SALREC - PROCESS SAL RECORDS (SALESPERSON)                          *         
***********************************************************************         
SALREC   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSALREC,R4                                                       
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    SAL010                                                           
         MVC   KEY,LASTKEY                                                      
         NI    STAT,X'FF'-CONTINUE                                              
         B     SAL020                                                           
SAL010   DS    0H                                                               
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,CURREP                                                  
         MVI   COMPLEN,24          DEFAULT KEY COMPARE LENGTH = 3               
SAL020   DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     SAL040                                                           
SAL030   DS    0H                                                               
         BAS   RE,SEQ                                                           
SAL040   DS    0H                                                               
         ZIC   R5,COMPLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   XIT                                                              
         DROP  R4                                                               
         BAS   RE,GETREC                                                        
         TM    OPTIONS,X'80'       CHECK SAL LEAVE DATE?                        
         BZ    SAL045                                                           
         OC    RSALLEAV,RSALLEAV                                                
         BZ    SAL045                                                           
         CLC   RSALLEAV,TODAY                                                   
         BL    SAL030                                                           
SAL045   DS    0H                                                               
         CLC   KEYWORD,SPACES      KEYWORD PROVIDED?                            
         BE    SAL050              NO                                           
         GOTO1 =V(REGENWLD),DMCB,(10,KEYWORD),(18,RSALNAME),RR=Y                
         TM    4(R1),X'80'                                                      
         BZ    SAL030                                                           
SAL050   DS    0H                                                               
         OC    BRWHEAD,BRWHEAD                                                  
         BNZ   SAL060                                                           
         MVC   BRWHEAD(16),=C'SALESPERSON NAME'                                 
         MVC   BRWHEAD+22(4),=C'CODE'                                           
         MVC   BRWHEAD+28(6),=C'OFFICE'                                         
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(16),DASHES                                                
         MVC   BRWUL2+22(4),DASHES                                              
         MVC   BRWUL2+28(6),DASHES                                              
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
SAL060   DS    0H                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RSALNAME),RSALNAME                                     
         MVC   RECLINE+22(L'RSALKSAL),RSALKSAL                                  
         MVC   RECLINE+30(2),RSALOFF                                            
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(3),RSALKSAL                                               
         MVC   RECBUF(L'RSALKSAL),RSALKSAL                                      
         MVC   RECBUF+L'GLBRWKW(L'RSALNAME),RSALNAME                            
         BAS   RE,DISPREC                                                       
         BE    SAL030                                                           
         B     XIT                                                              
***********************************************************************         
* PARACC - PAR ACCESS LEVEL LISTER                                    *         
***********************************************************************         
PARACC   NTR1                                                                   
         OC    PARPRG,SPACES                                                    
         CLC   PARPRG,SPACES                                                    
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE PROGRAM                            
         OC    PARREC,SPACES                                                    
*                                                                               
         MVC   BRWHEAD(12),=C'ACCESS LEVEL'                                     
         OI    BRWHEADH+6,X'80'                                                 
         MVC   BRWUL2(12),DASHES                                                
         OI    BRWUL2H+6,X'80'                                                  
         BAS   RE,SELHEAD                                                       
*                                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RSDFDLV),=CL8'ALL'                                     
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RSDFDLV),=CL8'ALL'                                      
         BAS   RE,DISPREC                                                       
*                                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RSDFDLV),=CL8'NOACCESS'                                
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RSDFDLV),=CL8'NOACCESS'                                 
         BAS   RE,DISPREC                                                       
*                                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RSDFDLV),=CL8'DEFAULT'                                 
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RSDFDLV),=CL8'DEFAULT'                                  
         BAS   RE,DISPREC                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSDFREC,R4                                                       
ACC010   DS    0H                                                               
         MVC   RSDFKTYP(2),=X'1502'                                             
         MVC   RSDFKREP,CURREP                                                  
         MVC   RSDFKPRG,PARPRG                                                  
         MVC   RSDFKREC,PARREC                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   XIT                                                              
         DROP  R4                                                               
         LA    R6,IO2                                                           
         ST    R6,AIOAREA                                                       
         GOTO1 GETREC              USE 4K IO AREA                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            FIRST DEFINITION ELEMENT                     
         BNE   XIT                                                              
         TM    STAT,CONTINUE       LIST CONTINUATION?                           
         BZ    ACC040                                                           
         NI    STAT,X'FF'-CONTINUE                                              
ACC020   DS    0H                                                               
         CLC   2(8,R6),LASTKEY     FIND WHICH ELEM WE LEFT OFF AT               
         BE    ACC040                                                           
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                SHOULD REALLY FIND IT                        
         CLI   0(R6),X'20'                                                      
         BE    ACC020                                                           
         DC    H'0'                                                             
ACC030   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         CLI   0(R6),X'20'                                                      
         BNE   XIT                                                              
         CLC   2(8,R6),KEY         SKIP CONTINUATION ELEMS                      
         BE    ACC030                                                           
ACC040   DS    0H                                                               
         MVC   KEY(8),2(R6)                                                     
*                                                                               
         XC    RECLINE,RECLINE                                                  
         MVC   RECLINE(L'RSDFDLV),2(R6)                                         
         XC    RECBUF,RECBUF                                                    
         MVC   RECBUF(L'RSDFDLV),2(R6)                                          
         BAS   RE,DISPREC                                                       
         BE    ACC030                                                           
         B     XIT                                                              
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
*                                                                               
         MVI   OPTIONS,0           DEFAULT NO OPTIONS                           
*                                                                               
         MVC   CURREP,REPALPHA     DEFAULT CURRENT REP                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RREPREC,R4                                                       
         MVI   RREPKTYP,1                                                       
         MVC   RREPKREP,CURREP                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC,DMCB,IOAREA                                               
         LA    R4,IOAREA                                                        
         MVC   MASTREP,RREPMAST                                                 
         DROP  R4                                                               
         OC    MASTREP,SPACES                                                   
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
         LA    R2,BRWRECLH         RECORD CODE FIELD LABEL                      
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
         LA    R2,BRWKEYWH         YES - MOVE KEYWORD FROM GLOBBER              
         MVC   8(10,R2),GLBRWKW         ELEM INTO RECTYPE FIELD                 
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         OC    GLBRWADV,GLBRWADV                                                
         BZ    SETUP40                                                          
         LA    R2,BRWADVH          YES - MOVE RECORD TYPE FROM GLOBBER          
         MVC   8(3,R2),GLBRWADV          ELEM INTO RECTYPE FIELD                
         MVI   5(R2),4                                                          
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
         DC    C'ADV',CL15'ADVERTISER',AL4(ADVREC)                              
         DC    C'AGY',CL15'AGENCY',AL4(AGYREC)                                  
         DC    C'SAL',CL15'SALESPERSON',AL4(SALREC)                             
         DC    C'PRD',CL15'PRODUCT',AL4(PRDREC)                                 
         DC    C'ACC',CL15'PAR ACCESS',AL4(PARACC)                              
         DC    C'TMP',CL15'BUY TEMPLATE',AL4(BUYTMP)                            
         DC    C'BCD',CL15'BUY CODE',AL4(BUYCOD)                                
         DC    C'STA',CL15'STATION',AL4(STAREC)                                 
         DC    C'MKT',CL15'MARKET CODE',AL4(MKTCOD)                             
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
MSGCODE  DC    C'REPPAK RECORD BROWSER - ENTER RECORD CODE && KEYWORD'          
MSGDISP  DC    C'REPPAK RECORD BROWSER - Select record or change keywor+        
               d'                                                               
MSGDIS2  DC   C'REPPAK RECORD BROWSER - View records or change keyword'         
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
       ++INCLUDE REGENINT                                                       
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
       ++INCLUDE RGENEROL                                                       
         EJECT                                                                  
       ++INCLUDE RGENOLD                                                        
         ORG   IOAREA+1008                                                      
         SPACE 2                                                                
BASERB   DS    F                                                                
BASERD   DS    F                                                                
RELO     DS    A                   RELOCATION FACTOR - ROOT PHASE               
RELO1    DS    A                   RELO - FIRST LEVEL OVERLAY                   
SYSFAC   DS    A                   A(SYSTEM FACILITIES)                         
ACOMFACS DS    A                   A(COMFACS)                                   
GLOBBER  DS    A                   A(GLOBBER)                                   
GETTXT   DS    A                   A(GETTXT)                                    
PTRBUF   DS    A                   NEXT AVAILABLE BUFFER ENTRY                  
PTRREC   DS    A                   NEXT AVAILABLE REC DISPLAY LINE              
LINENUM  DS    X                   CURRENT DISPLAY LINE NUMBER                  
ELCODE   DS    C                                                                
LWORK    EQU   *-DMWORK                                                         
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENSDF                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENTMP                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENSPEC                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE REBRWFFD                                                       
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
KEYWORD  DS    CL10                KEYWORD ARGUMENT                             
COMPLEN  DS    X                   KEY COMPARE LENGTH                           
RECLINE  DS    CL(L'BRWREC1)       RECORD DISPLAY LINE BUILD AREA               
RECBUF   DS    CL(L'GLBRWKW+L'GLBRWRD)    BUFFER ENTRY BUILD AREA               
LASTKEY  DS    CL(L'KEY)                                                        
TODAY    DS    CL3                 TODAY'S DATE                                 
SRCEREP  DS    CL2                 SOURCE REP                                   
SRCEUTL  DS    X                   SOURCE FILE                                  
ORIGUTL  DS    X                   ORIGINAL FILE                                
CURREP   DS    CL2                 CURRENT REP                                  
MASTREP  DS    CL2                 MASTER REP FIELD FROM RREPREC                
PRDADV   DS    CL4                 ADVERTISER CODE (FOR PRD REC LOOKUP)         
PARPRG   DS    CL3                 PAR ACCESS LEVEL LOOKUP - PROGRAM            
PARREC   DS    CL8                 PAR ACCESS LEVEL LOOKUP - RECORD             
*                                                                               
OPTIONS  DS    X                   RECORD OPTIONS FROM CALLER PROG              
**GUIDE TO RECORD OPTIONS**********************************************         
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
LINES    EQU   18                  # OF DISPLAY LINES ON SCREEN                 
BUFFER   DS    (LINES)CL(L'GLBRWKW+L'GLBRWRD)                                   
IO2      DS    XL4096              4K IOAREA                                    
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE FATWA                                                          
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLBRW                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068REBRW00S  11/21/02'                                      
         END                                                                    
