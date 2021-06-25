*          DATA SET ACTRA03    AT LEVEL 053 AS OF 02/15/19                      
*PHASE T62203B                                                                  
         TITLE '(T62203)  BILLING TRANSFER - LIST OVERLAY'                      
T62203   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T62203**,RR=RE                                                 
*                                                                               
         USING TWAD,R5            R5=A(TWA)                                     
         USING SAVAREA,R6         R6=A(SAVE AREA)                               
         USING WORKD,R7           R7=A(GLOBAL WORKING STORAGE)                  
         L     RC,APALOCAL        RC=A(LOCAL WORKING STORAGE)                   
         USING LOCALD,RC                                                        
*                                                                               
         LA    R2,IOKEY                                                         
         USING MPRRECD,R2                                                       
*                                                                               
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO             RELOCATION FACTOR                          
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   APMODE,APMVALP        VALIDATE KEY FOR LIST                      
         BE    VALPAR                                                           
         CLI   APMODE,APMGETS        GET RECORD FOR LIST                        
         BE    GETSEL                                                           
         CLI   APMODE,APMDISS        DISPLAY RECORD FOR LIST                    
         BE    DISSEL                                                           
         CLI   APMODE,APMDISK        DISPLAY KEY FOR SELECT                     
         BE    DISKEY                                                           
*                                                                               
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
YES      CR    RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*==================================*                                            
* VALPAR  - VALIDATE KEY FOR LIST  *                                            
*==================================*                                            
*                                                                               
VALPAR   LA    R2,APRECKEY                                                      
         XC    APRECKEY,APRECKEY  BUILD KEY                                     
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         GOTO1 AVALSYS,LISSYSH    VALIDATE SYSTEM                               
         BNE   VALPX                                                            
         MVC   MPRKALPH,QALPH     AGY ALPHA FOR SPLIT MED FILE(IF SET)          
         MVC   MPRKSYS,QSYS       SYSTEM                                        
*                                                                               
         GOTO1 AVALMED,LISMEDH    VALIDATE MEDIA                                
         BNE   VALPX                                                            
         MVC   MPRKMED,QMED       SET MEDIA IN KEY                              
         OC    QMED,QMED          IF NO MEDIA                                   
         BNZ   VALOFF                                                           
         GOTO1 AVALFLD,APPARM,LISOFFH,2                                         
         CLI   APPARM,X'FF'       NO OFFICE/CLIENT                              
         BNE   VALOFF                                                           
         MVC   FVMSGNO,=AL2(FVIMISS)                                            
         B     VALPX                                                            
*                                                                               
VALOFF   MVI   OCFLAG,0           PRE-CLEAR OFFICE INDICATOR                    
         GOTO1 AVALOFF,LISOFFH    VALIDATE OFFICE                               
         BNE   VALPX                                                            
         OC    QOFF,QOFF          IF OFFICE                                     
         BZ    VALPCLT                                                          
         MVI   OCFLAG,C'O'        MARK OFFICE INPUTTED                          
         MVC   MPRKOFC,QOFF                                                     
*        MVI   MPRKOFC,X'5B'      READ HIGH FOR OFFICE GROUPS ($)               
*        CLI   QOFF,C'$'                                                        
*        BE    *+8                                                              
*        MVI   MPRKOFC,X'5C'      READ HIGH PAST OFFICE GROUPS                  
*                                                                               
VALPCLT  GOTO1 AVALCLT,LISCLTH    VALIDATE CLIENT                               
         BNE   VALPX                                                            
         OC    QCLT,QCLT          IF CLIENT                                     
         BZ    VALPAR20                                                         
         CLI   OCFLAG,C'O'        CAN'T HAVE OFFICE TOO                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPX                                                            
         MVI   OCFLAG,C'C'        SET CLIENT REQUESTED ALONE                    
         MVC   MPRKCLI,QCLT       SET CLIENT IN KEY FOR READ HIGH               
*                                                                               
VALPAR20 GOTO1 ASETFILE           SET ACC FILE & LIMIT ACCESS (IF NEC)          
         BE    VALPAR40                                                         
         LA    R2,LISSYSH                                                       
         ST    R2,APCURSOR                                                      
         B     VALPX                                                            
*                                                                               
VALPAR40 MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   SELKEY,APRECKEY                                                  
         LA    RE,LISSL1H         SET APPARM FIELDS FOR CONTROLLER              
         ST    RE,APPARM          A(FIRST TWA SELECT FIELD HEADER)              
         LA    RE,14              # NUMBER OF LINES ON SCREEN                   
         STC   RE,APPARM+4                                                      
         LA    RE,LISSL2H-LISSL1H                                               
         STCM  RE,3,APPARM+6      LENGTH OF SELECT LINE                         
VALPX    B     EXIT                                                             
         EJECT                                                                  
*=======================================*                                       
* GETSEL - GET RECORD FOR LIST DISPLAY  *                                       
*=======================================*                                       
*                                                                               
GETSEL   LA    R2,IOKEY                                                         
         MVC   MPRKEY,APRECKEY                                                  
         CLI   APINDS,APILFLST    IF FIRST LINE OF FIRST SCREEN                 
         BNE   *+8                                                              
         MVI   MPRKTYP,0          ENSURE RESET OF KEY                           
*                                                                               
         CLI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         BNE   GETSEL10                                                         
         CLI   MPRKSUB,MPRKSUBQ   X'01'                                         
         BE    GETSEL20                                                         
GETSEL10 XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPRKALPH,QALPH     AGY ALPHA FOR MED SPLIT FILES                 
         MVC   MPRKSYS,QSYS                                                     
         MVC   MPRKMED,QMED                                                     
         B     GETSEL60                                                         
*                                                                               
GETSEL20 TM    APINDS,APILRERD    TEST RE-READ RECORD?                          
         BZ    GETSEL40                                                         
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO1,=C'SE1'                         
         BE    GETSEL70           ESTABLISH READ SEQUENCE                       
         B     GETSEL95                                                         
*                                                                               
GETSEL40 TM    APINDS,APILNSEQ    CONTINUE WITH SEQUENCIAL READ?                
         BO    GETSEL70                                                         
GETSEL60 GOTO1 AMIOACC,APPARM,IOACCFIL+IOHI+IO1,=C'SE1'                         
         CLI   MYIOERR,0                                                        
         BNE   GETSEL95                                                         
         B     GETSEL75                                                         
*                                                                               
GETSEL70 GOTO1 AMIOACC,APPARM,IOACCFIL+IOSEQ+IO1,=C'SE1'                        
         CLI   MYIOERR,0                                                        
         BNE   GETSEL95                                                         
*                                                                               
GETSEL75 L     R2,AIOAREA1                                                      
         CLI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         BNE   GETSEL95                                                         
         CLI   MPRKSUB,MPRKSUBQ   X'01'                                         
         BNE   GETSEL95                                                         
         CLC   MPRKALPH,QALPH     SAME AGY ALPHA                                
         BNE   GETSEL95                                                         
         CLC   MPRKCPY,COMPANY    SAME NATIVE COMPANY                           
         BNE   GETSEL95                                                         
         CLC   MPRKSYS,QSYS       SAME SYSTEM                                   
         BNE   GETSEL95                                                         
*                                                                               
         OC    QMED,QMED          REQUESTED A PARTICULAR MEDIA?                 
         BZ    *+14               NO - ALL MEDIAS                               
         CLC   MPRKMED,QMED       YES - MUST MATCH                              
         BNE   GETSEL70                                                         
         OC    QOFF,QOFF          REQUESTED A PARTICULAR OFFICE ONLY?           
         BZ    *+12               NO - ALL OFFICES                              
         BAS   RE,CHKCLT          CHECK CLIENT VALID FOR OFF CODE/GRP           
         BNE   GETSEL70                                                         
         OC    QCLT,QCLT          REQUESTED A PARTICULAR CLIENT ONLY?           
         BZ    *+14               NO - ALL CLIENTS                              
         CLC   MPRKCLI,QCLT       YES - MUST MATCH                              
         BNE   GETSEL70                                                         
*                                                                               
         MVC   APWORK,SPACES                                                    
         OC    INODISP,INODISP                                                  
         BZ    *+8                                                              
         BAS   RE,SETREC          SET RECORD TYPE LOOKING FOR                   
         BAS   RE,GETELE          SEE IF ELE EXISTS IN RECORD                   
         BNE   GETSEL70                                                         
         BAS   RE,GETTYPE         SET RECORD TYPE IN TABLE                      
         BAS   RE,SETEXP          SET EXCEPTION                                 
*                                                                               
GETSEL90 MVC   APRECKEY(L'MPRKEY),MPRKEY                                        
         MVC   SVKEY,MPRKEY       SAVE CURRENT KEY                              
         GOTO1 AMIOACC,APPARM,IOACCFIL+IOSEQ+IO1,=C'SE1'                        
         CLI   MYIOERR,0                                                        
         BNE   GETSEL95                                                         
         CLC   0(L'MPRKEY,R2),SVKEY       SAME KEY BUT DIFF TYPE?               
         BNE   GETSEL92                   YES - CHECK ELE MATCH                 
         BAS   RE,GETELE                                                        
         BNE   GETSEL90                                                         
         BAS   RE,GETTYPE                                                       
         BAS   RE,SETEXP          SET EXCEPTION                                 
         B     GETSEL90                                                         
*                                                                               
GETSEL92 MVC   IOKEY(L'SVKEY),SVKEY       RESET KEY                             
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO1,=C'SE1'                         
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
******** OI    APINDS,APILRERD                                                  
         OC    INODISP,INODISP                                                  
         BZ    GETSELX                                                          
         CLC   APWORK,SPACES                                                    
         BNE   GETSELX                                                          
         XC    TBLTYPE,TBLTYPE                                                  
         B     GETSEL70                                                         
*                                                                               
GETSEL95 MVI   APMODE,APMEOFS     TELL GENERAL NO MORE LINES                    
GETSELX  B     EXIT                                                             
         EJECT                                                                  
*        ROUTINE TO CHECK CLIENT OKAY TO LIST                                   
*                                                                               
CHKCLT   NTR1                                                                   
         OC    MPRKOFC,MPRKOFC     IF OFFICE SPECIFIED IN KEY                   
         BZ    CHKCLT3                                                          
         CLI   MPRKOFC,C'$'        AND ITS AN OFFICE CODE                       
         BE    CHKCLT2                                                          
         CLI   QOFFIND,C'O'        AND OFFICE CODE IN REQUEST                   
         BNE   *+18                                                             
         CLC   MPRKOFC(1),QOFF     MUST MATCH EXACTLY                           
         BE    YES                                                              
         B     NO                                                               
         BAS   RE,CALLOFF          CHK OFF CODE BELONGS TO GRP REQ'D            
         B     EXIT                                                             
*                                                                               
CHKCLT2  DS    0H                  IF OFFICE GROUP IN KEY                       
         CLI   QOFFIND,C'G'        AND OFFICE GROUP IN REQUEST                  
         BNE   NO                                                               
         CLC   MPRKOFC,QOFF        MUST MATCH EXACTLY                           
         B     EXIT                                                             
*                                                                               
CHKCLT3  OC    MPRKCLI,MPRKCLI     IF CLIENT IN KEY                             
         BZ    NO                                                               
*                                                                               
         CLI   QSYS,C'P'           GET PACKED CLIENT FOR SPOT/NET               
         BE    CHKCLT4                                                          
         GOTO1 VCLPACK,APPARM,MPRKCLI,APWORK                                    
CHKCLT4  L     R3,ACLITAB                                                       
*                                                                               
CHKCLT5  OC    0(3,R3),0(R3)       IF NOT END OF TABLE                          
         BZ    NO                                                               
         CLI   QSYS,C'P'           IF PRINT                                     
         BNE   CHKCLT7                                                          
         CLC   MPRKCLI,0(R3)       CHECK AGAINST CHARACTER CLIENT CODE          
         BE    YES                                                              
         B     CHKCLT9                                                          
*                                                                               
CHKCLT7  CLC   APWORK(2),0(R3)     CHECK AGAINST PACKED CLIENT CODE             
         BE    YES                                                              
*                                                                               
CHKCLT9  LA    R3,3(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         B     CHKCLT5             LOOP                                         
         EJECT                                                                  
CALLOFF  NTR1                                                                   
         XC    APDUB,APDUB         SET UP PARAMETER BLOCK                       
         LA    R1,APDUB                                                         
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,QOFF                                                     
         MVC   OFCOFC,MPRKOFC                                                   
         MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH                                                     
         DROP  R1                                                               
         GOTO1 VOFFICER,APPARM,APDUB,ACOM                                       
         CLI   0(R1),0                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SET RECORD TYPE BASED ON SELECT CODE                  
*                                                                               
SETREC   NTR1                                                                   
         LA    RE,SELCTAB          RE=A(SELECT CODES/RECORD TYPE)               
*                                                                               
SETREC5  CLI   0(RE),X'FF'         WHILE NOT END OF TABLE                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RE),INODISP     IF MATCH ON SELECT                           
         BE    *+12                                                             
         LA    RE,L'SELCTAB(RE)                                                 
         B     SETREC5                                                          
*                                                                               
         MVC   APBYTE,1(RE)        SET RECORD EQUATE                            
         B     EXIT                                                             
         SPACE 2                                                                
* GETTYPE - SET UP TABLE OF RECORDS - X IN SLOT MEANS RECORD EXISTS             
*                                                                               
GETTYPE  NTR1                                                                   
         LA    RE,TBLTYPE          RE=A(TABLE)                                  
         USING TYPD,RE                                                          
*                                                                               
         ZIC   R1,MPRKPRO          RECORD EQUATE                                
         CLI   MPRKPRO,MPRKAOR     IF AOR RECORD                                
         BNE   GETTYPE5                                                         
         TM    APSTTYPE,APSTAOR2  FOR APOS2                                     
         BZ    *+8                                                              
         MVI   TYPIORA,C'X'        STICK IT AT END OF TABLE                     
         TM    APSTTYPE,APSTAOR                                                 
         BZ    GETTYPEX                                                         
         SPACE 1                                                                
GETTYPE5 BCTR  R1,0                ELSE JUST GO BY RECORD EQUATE                
         AR    RE,R1                                                            
         MVI   0(RE),C'X'          MARK IT WITH AN X                            
GETTYPEX B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*=======================================================*                       
* GETELE  - EX IF POST/LIST - MAKES SURE A POSTING ELE  *                       
*         - EXISTS IN REC + IF INODISP SET CHECKS THAT  *                       
*         - THAT PARITCULAR ELE EXISTS                  *                       
*=======================================================*                       
*                                                                               
GETELE   NTR1                                                                   
         CLI   INREC,RECPROF      LISTING PROFILE RECORDS?                      
         BE    GETE30             YES - CHECK IF PROFILE ELEMENT EXISTS         
         LR    R1,R2                                                            
         AH    R1,DATADISP                                                      
         USING MBTELD,R1                                                        
         MVI   APSTTYPE,0                                                       
*                                                                               
GETE2    CLI   0(R1),0            END OF RECORD                                 
         BNE   GETE5                                                            
         CLI   APSTTYPE,0                                                       
         BE    NO                                                               
         B     YES                                                              
*                                                                               
GETE5    CLI   0(R1),MBTELQ       NO - DOES POST ELEMENT EXIST                  
         BNE   GETE10                                                           
         CLI   MPRKPRO,MPRKAOR    IF APOST RECORD                               
         BNE   YES                                                              
         CLI   MBTTYP,MBTTINTI     MUST HAVE ELEM OTHER THAN INTERNAL           
         BL    *+12                                                             
         CLI   MBTTYP,MBTTINTR                                                  
         BNH   *+12                                                             
         OI    APSTTYPE,APSTAOR                                                 
         B     *+8                                                              
         OI    APSTTYPE,APSTAOR2                                                
*                                                                               
GETE10   BAS   RE,GETNXT                                                        
         B     GETE2                                                            
         DROP  R1                                                               
*                                                                               
GETE30   LR    R1,R2                                                            
         AH    R1,DATADISP                                                      
         USING MTPELD,R1                                                        
GETE35   CLI   0(R1),0            END OF RECORD                                 
         BE    NO                 YES - NO MATCH - RESET & EXIT                 
         CLI   0(R1),MTPELQ       LISTING PROFILE- DOES ELEMENT EXIST           
         BE    YES                                                              
         BAS   RE,GETNXT                                                        
         B     GETE35                                                           
         DROP  R1                                                               
         SPACE                                                                  
GETNXT   SR    R0,R0              GET NEXT ELEMENT IN RECORD                    
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
SETEXP   NTR1                                                                   
         OC    INODISP,INODISP                                                  
         BZ    SETEX                                                            
         CLC   APBYTE,MPRKPRO     CHECK RECORD TYPE                             
         BNE   SETEX                                                            
         CLI   INREC,RECPROF      LISTING PROFILE RECORDS?                      
         BE    SETE30             YES - CHECK IF PROFILE ELEMENT EXISTS         
         LR    R1,R2                                                            
         AH    R1,DATADISP                                                      
         USING MBTELD,R1                                                        
SETE2    CLI   0(R1),0            END OF RECORD                                 
         BE    SETEX              YES - NO MATCH                                
         CLI   0(R1),MBTELQ       NO - DOES POST ELEMENT EXIST                  
         BNE   SETE22                                                           
         CLC   MBTTYP,INODISP+1   CHECK ELE TYPE                                
         BNE   SETE22             GET NEXT ELEMENT                              
         MVC   APFULL,INOPTI                                                    
         NC    APFULL,=A(OPTACCB) LOOKING FOR ACCT=                             
         BZ    SETE10                                                           
         OC    MBTULA,MBTULA                                                    
         BZ    SETE22             GET NXT ELEMENT                               
         MVC   APWORK(L'MBTULA),MBTULA                                          
         B     SETEX                                                            
*                                                                               
SETE10   MVC   APFULL,INOPTI      LOOKING FOR AMT=                              
         NC    APFULL,=A(OPTAMTB)                                               
         BZ    SETE15                                                           
         OC    MBTAMTX,MBTAMTX                                                  
         BZ    SETE22             GET NXT ELEMENT                               
         MVC   APWORK(L'MBTAMTX),MBTAMTX                                        
         B     SETEX                                                            
*                                                                               
SETE15   OC    MBTMEMOX,MBTMEMOX                                                
         BZ    SETE22             GET NXT ELEMENT                               
         MVC   APWORK(L'MBTMEMOX),MBTMEMOX                                      
         B     SETEX                                                            
*                                                                               
SETE22   BAS   RE,SETNXT                                                        
         B     SETE2                                                            
         DROP  R1                                                               
*                                                                               
SETE30   LR    R1,R2                                                            
         AH    R1,DATADISP                                                      
         USING MTPELD,R1                                                        
SETE35   CLI   0(R1),0            END OF RECORD                                 
         BE    SETEX              YES - NO MATCH - RESET & EXIT                 
         CLI   0(R1),MTPELQ       LISTING PROFILE- DOES ELEMENT EXIST           
         BNE   SETE40                                                           
         CLC   MTPFNUM,INODISP+1  CHECK ELE TYPE                                
         BNE   SETE40                                                           
         CLI   MTPFNUM,MTPFIPCT    IF PERCENTAGE                                
         BE    *+12                                                             
         CLI   MTPFNUM,MTPFIPT2                                                 
         BNE   SETE37                                                           
         EDIT  (4,MTPFDATA),(5,APWORK),2,ALIGN=LEFT,ZERO=NOBLANK,      X        
               DUB=APDUB,WRK=APWORK2                                            
         B     SETEX                                                            
SETE37   ZIC   RE,MTPLN           SAVE EXCEPTION                                
         LA    RF,MTPFDATA-MTPEL                                                
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),MTPFDATA                                               
         B     SETEX                                                            
         DROP  R1                                                               
SETE40   BAS   RE,SETNXT                                                        
         B     SETE35                                                           
*                                                                               
SETNXT   SR    R0,R0              GET NEXT ELEMENT IN RECORD                    
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         BR    RE                                                               
*                                                                               
SETEX    XIT1                                                                   
         EJECT                                                                  
*====================================*                                          
* DISSEL RECORD                      *                                          
* INPUT APPARM(4) =A (TWA DISP LINE) *                                          
*====================================*                                          
*                                                                               
DISSEL   L     R4,APPARM          R2=A(TWA DISPLAY LINE)                        
         USING LDISD,R4                                                         
         L     R2,AIOAREA1                                                      
         MVC   LMED,MPRKMED       MOVE MEDIA TO SCREEN                          
         GOTO1 ADISOFF,APPARM,MPRKOFC,LOFF                                      
*                                                                               
         MVC   LCLT,MPRKCLI       MOVE CLIENT CODE                              
         MVC   LPRD,MPRKPRD       MOVE PRODUCT                                  
*                                                                               
         LA    R3,TBLTYPE         TABLE OF TYPES FOR THIS RECORD                
         CLI   INREC,RECPOST                                                    
         BE    DISSEL5                                                          
         BAS   RE,DISPPROF                                                      
         B     DISSEL10                                                         
DISSEL5  BAS   RE,DISPPOST                                                      
*                                                                               
DISSEL10 XC    TBLTYPE,TBLTYPE    CLEAR TABLE TYPE - FOR NEXT REC               
DISSELX  B     EXIT                                                             
         EJECT                                                                  
*              DISPLAY RECORDS THAT EXIST OF POST TYPE                          
*                                  R3=A(TBLTYPE TABLE)                          
         USING TYPD,R3                                                          
DISPPOST NTR1                                                                   
         MVC   LPOST+1(1),TYPREG                                                
         MVC   LUCPOST(1),TYPUFC                                                
         MVC   LUNPOST(1),TYPUFN                                                
         MVC   LAPOST+1(1),TYPAOR                                               
         MVC   LAPOS2(1),TYPIORA                                                
         MVC   LUACPOST+1(1),TYPAUFC                                            
         MVC   LUANPOST+1(1),TYPAUFN                                            
         MVC   LRPOST+1(1),TYPRET                                               
         MVC   LTPOST+1(1),TYPPST                                               
         MVC   LSPOST+1(1),TYPSPP                                               
         MVC   LDPOST+1(1),TYPDIF                                               
         MVC   LPSTVAL,APWORK                                                   
         B     EXIT                                                             
         SPACE                                                                  
*              DISPLAY RECORDS THAT EXIST OF PROF TYPE                          
*                                                                               
DISPPROF NTR1                                                                   
         MVC   LPROF+1(1),TYPREG                                                
         MVC   LRPROF+1(1),TYPRET                                               
         MVC   LSPROF+1(1),TYPSPP                                               
         MVC   LPRFVAL,APWORK                                                   
         B     EXIT                                                             
         DROP  R4,R3                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISKEY - DISPLAY RECORD SELECTED                                              
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         MVC   POSSYS(1),MPRKSYS     DISPLAY SYSTEM                             
         MVC   POSSYS+1(2),MPRKALPH  DISPLAY AGY ALPHA FOR SPLIT FILES          
         OI    POSSYSH+6,X'80'                                                  
         MVC   POSMED,MPRKMED     DISPLAY MEDIA                                 
         OI    POSMEDH+6,X'80'                                                  
         GOTO1 ADISOFF,APPARM,MPRKOFC,POSOFF                                    
         BNE   *+8                                                              
         OI    POSOFFH+6,X'80'                                                  
         MVC   POSCLT,MPRKCLI     DISPLAY CLIENT                                
         OI    POSCLTH+6,X'80'                                                  
         MVC   POSPRD,MPRKPRD     DISPLAY PRODUCT                               
         OI    POSPRDH+6,X'80'                                                  
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* LITERAL POOL                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
         SPACE                                                                  
*              TABLE OF SELECT CODE TO RECORD TYPE                              
*                                                                               
SELCTAB  DS    0CL2                                                             
         DC    C'P',AL1(MPRKREG)                                                
         DC    C'A',AL1(MPRKAOR)                                                
         DC    C'I',AL1(MPRKAOR)                                                
         DC    C'R',AL1(MPRKRTL)                                                
         DC    C'S',AL1(MPRKPPB)                                                
         DC    C'C',AL1(MPRKUC)                                                 
         DC    C'N',AL1(MPRKUN)                                                 
         DC    C'1',AL1(MPRKUAC)                                                
         DC    C'2',AL1(MPRKUAN)                                                
         DC    C'T',AL1(MPRKPST)                                                
         DC    C'D',AL1(MPRKDIF)                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* ACTRAWRK                                                                      
*                                                                               
       ++INCLUDE ACTRAWRK                                                       
         EJECT                                                                  
LOCALD   DSECT                                                                  
OCFLAG   DS    CL1                O= OFFICE INPUTTED                            
RECHANG  DS    CL1                                                              
NEWREC   DS    CL1                                                              
APSTTYPE DS    XL1                                                              
APSTAOR  EQU   X'80'                                                            
APSTAOR2 EQU   X'40'                                                            
LCHDT    DS    XL3                TODAYS BINARY DATE                            
TRACTBL  DS    (MAXPOSTS)XL2                                                    
SELKEY   DS    CL(L'MPRKEY)                                                     
SVKEY    DS    CL(L'MPRKEY)                                                     
APWORK2  DS    CL(L'APWORK)                                                     
TBLTYPE  DS    CL11                                                             
         EJECT                                                                  
*                                                                               
* TYPED  DSECT TO COVER TBLTYPE                                                 
*                                                                               
TYPD     DSECT                                                                  
TYPREG   DS    XL1                                                              
TYPAOR   DS    XL1                                                              
TYPRET   DS    XL1                                                              
TYPSPP   DS    XL1                                                              
TYPUFC   DS    XL1                                                              
TYPUFN   DS    XL1                                                              
TYPAUFC  DS    XL1                                                              
TYPAUFN  DS    XL1                                                              
TYPPST   DS    XL1                                                              
TYPDIF   DS    XL1                                                              
TYPIORA  DS    XL1                                                              
         EJECT                                                                  
*                                                                               
* DISD - DSECT TO COVER DISPLAY SCREEN                                          
*                                                                               
DISD     DSECT                                                                  
DDISH    DS    CL8                FOR HEADER                                    
DDIS     DS    CL10               DISCRIPTION                                   
DACCH    DS    CL8                FOR HEADER                                    
DACC     DS    CL14               ACCOUNT                                       
DACCLVLH DS    CL8                FOR HEADER                                    
DACCLVL  DS    CL3                ACCOUNT LEVEL                                 
DAMTH    DS    CL8                FOR HEADER                                    
DAMT     DS    CL8                AMOUNT                                        
DMEMOH   DS    CL8                FOR HEADER                                    
DMEMO    DS    CL8                MEMO                                          
DLVLH    DS    CL8                FOR HEADER                                    
DLVL     DS    CL3                LEVEL                                         
DLSTACTH DS    CL8                FOR HEADER                                    
DLSTACT  DS    CL13               LAST ACTIVITY                                 
DISDL    EQU   *-DDISH            LENGTH OF ONE DISPLAY LINE                    
         EJECT                                                                  
*                                                                               
* LISD - DSECT TO COVER LIST SCREEN                                             
*                                                                               
LDISD    DSECT                                                                  
         DS    CL8                HEADER FOR SELECT                             
         DS    CL3                SELECT FIELD                                  
         DS    CL8                HEADER FOR LIST LINE                          
LMED     DS    CL1                MEDIA                                         
         DS    CL1                SPACE                                         
LOFF     DS    CL3                OFFICE CODE OR GROUP                          
*        DS    CL1                                                              
         DS    CL1                SPACE                                         
LCLT     DS    CL3                CLIENT                                        
         DS    CL1                                                              
LPRD     DS    CL3                PRODUCT                                       
         DS    CL1                                                              
LPOST    DS    CL3                POST RECORD                                   
         DS    CL1                                                              
LUCPOST  DS    CL2                UPFRONT COMMISSION POST                       
         DS    CL1                                                              
LUNPOST  DS    CL2                UPFRONT NET POST                              
         DS    CL1                                                              
LAPOST   DS    CL3                APOST RECORD                                  
         DS    CL1                                                              
LAPOS2   DS    CL2                APOS2 RECORD                                  
         DS    CL1                                                              
LUACPOST DS    CL3                AOR UPFRONT COMMISSION POST                   
         DS    CL1                                                              
LUANPOST DS    CL3                AOR UPFRONT NET POST                          
         DS    CL1                                                              
LRPOST   DS    CL3                RPOST RECORD                                  
         DS    CL1                                                              
LTPOST   DS    CL3                TPOST RECORD                                  
         DS    CL1                                                              
LSPOST   DS    CL3                SPOST RECORD                                  
         DS    CL1                                                              
LDPOST   DS    CL3                DPOST RECORD                                  
         DS    CL1                                                              
LPSTVAL  DS    CL14               POSTING ACCOUNT                               
         ORG   LPOST                                                            
LPROF    DS    CL3                PROF RECORD                                   
         DS    CL1                                                              
LRPROF   DS    CL3                RPROF RECORD                                  
         DS    CL1                                                              
LSPROF   DS    CL3                SPROF RECORD                                  
         DS    CL5                                                              
LPRFVAL  DS    CL20               PROFILE VALUE                                 
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFDD                                                       
         SPACE 2                                                                
         ORG                                                                    
         SPACE 2                                                                
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFED                                                       
         SPACE 2                                                                
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053ACTRA03   02/15/19'                                      
         END                                                                    
