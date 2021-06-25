*          DATA SET TAREP5B    AT LEVEL 002 AS OF 10/23/14                      
*PHASE T7035BA                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T7035B - SEND SUMMARY OF DAYS TRANSACTIONS TO MQ'               
T7035B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T7035B,R6                                                 
         LR    RE,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING RECD,R7             R7=A(LOCAL W/S)                              
         ST    RE,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
         AHI   RE,L'SVPTRBLK                                                    
         ST    RE,AUPPTRS          SAVE A(UPDATED POINTER BLOCK)                
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         CLI   MODE,PRINTREP       IF CALLED WITH PRINTREP                      
         JNE   XIT                                                              
                                                                                
         CLC   TGUSER,=H'15778'    EXIT IF USER-ID IS TPTPC                     
         JE    XIT                                                              
         CLC   TGUSER,=H'15777'    OR USER-ID IS TPCLI                          
         JE    XIT                                                              
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         BRAS  RE,TOCERNO          SUMMARIZE NOTICES SENT TO CERNO              
                                                                                
         BRAS  RE,FRCERNO          SUMMARIZE TRANSACTIONS FROM CERNO            
                                                                                
         BRAS  RE,FRVITA           SUMMARIZE TRANSACTIONS FROM VIA              
                                                                                
         BRAS  RE,UPDCGS           UPDATE COMMERCIAL CLIENT GROUPS              
                                                                                
         BRAS  RE,UPDLDAT          UPDATE LAST RUN DATE                         
                                                                                
         BRAS  RE,TCCFILEN         AND CLEAR DAY'S FILENAMES                    
                                                                                
***********************************************************************         
*        CONDITION CODE AND EXIT ROUTINE                              *         
***********************************************************************         
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SUMMARIZE NOTICES SENT TO CERNO                   *         
***********************************************************************         
                                                                                
TOCERNO  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,TCINIT           INITIALIZE                                   
                                                                                
         USING TLSYD,R4                                                         
TC10     LA    R4,KEY                                                           
         XC    KEY,KEY             GET HOLDING FEE WEB FILENAMES RECORD         
         MVI   TLSYCD,TLSYCDQ                                                   
         MVI   TLSYTYPE,TLSYHFFN                                                
         GOTO1 HIGH                                                             
         J     TC30                                                             
TC20     GOTO1 SEQ                                                              
TC30     CLC   TLSYKEY(TLSYSEQ-TLSYD),KEYSAVE                                   
         JNE   TC80                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         GOTO1 DATCON,DMCB,(1,CURRDAT1),(8,WORK)                                
         GOTO1 DATVAL,DMCB,WORK,DUB                                             
         GOTO1 DATCON,DMCB,(0,DUB),(20,CURRDAT8)                                
                                                                                
TC40     BAS   RE,TCGFILEN         GET DAY'S FIRST/NEXT XML FILENAME            
         JNE   TC20                                                             
                                                                                
         XR    R3,R3               R3=MQ SUCCESSFULLY CLOSED COUNTER            
                                                                                
         BAS   RE,TCOPENIN         OPEN XML FILE FOR COUNTING                   
         JNE   TC40                                                             
                                                                                
         USING FILDATAD,R2                                                      
         LA    R2,FILDATA                                                       
TC50     GET   INPFILE,FILDATA     GET ENTRY FROM INPUT FILE                    
                                                                                
         CLC   MQSCLOS,FILENTRY    IF ENTRY IS "MQ SUCCESSFULLY CLOSED"         
         JNE   TC50                INCREASE COUNTER                             
         AHI   R3,1                                                             
         J     TC50                                                             
                                                                                
TC60     BAS   RE,TCCLOSIN         WHEN DONE, CLOSE FILE                        
                                                                                
         LTR   R3,R3               IF NO ENTRIES WERE SUCCESSFULLY              
         JZ    TC40                SENT TO MQ, GET NEXT FILE                    
                                                                                
         BAS   RE,TCOPENIN         OPEN FILE FOR INPUT                          
         JNE   TC40                                                             
                                                                                
         USING FILDATAD,R2                                                      
         LA    R2,FILDATA                                                       
TC70     GET   INPFILE,FILDATA     GET ENTRY FROM INPUT FILE                    
                                                                                
         BAS   RE,TCID2MQ          SEND DOCUMENT IDS TO MQ                      
                                                                                
         CLC   MQSCLOS,FILENTRY    IF ENTRY IS "MQ SUCCESSFULLY CLOSED"         
         JNE   TC70                DECREASE COUNTER                             
         BCT   R3,TC70             GO GET NEXT ENTRY                            
         DROP  R2                                                               
                                                                                
         BAS   RE,TCCLOSIN         WHEN DONE, CLOSE FILE                        
         J     TC40                AND GO READ NEXT                             
                                                                                
TC80     BAS   RE,TCCLOMQ          WHEN ALL FILES PROCESSED, CLOSE MQ           
                                                                                
         CLC   TGTODAY1,CURRDAT1                                                
         JNH   XIT                                                              
         GOTO1 DATCON,DMCB,(1,CURRDAT1),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK,1                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,CURRDAT1)                                
         J     TC10                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE VARIABLES, RECORDS AND TABLES          *         
***********************************************************************         
                                                                                
TCINIT   NTR1                                                                   
         XC    RECD(RECDLNQ),RECD  AND SETUP LOCAL VARIABLES                    
         MVC   FILENAM,SPACES                                                   
         MVI   DATETAB,X'FF'                                                    
         DROP  RA                                                               
                                                                                
         BRAS  RE,SETFDAT          SET FIRST DATE TO READ FOR                   
                                                                                
         USING CONHEADH-64,RE                                                   
         L     RE,ATWA                                                          
                                                                                
         USING TWADCOND,R1                                                      
         L     R1,TWADCONS                                                      
         MVC   DYNALLOC,TDYNALLO   ADDRESS OF DYNAMIC ALLOCATION ROUT.          
         J     XIT                                                              
         DROP  R1,RE                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET DAY'S FIRST/NEXT XML BACKUP FILE              *         
***********************************************************************         
                                                                                
TCGFILEN NTR1                                                                   
         USING TACMD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACMELQ                                                   
         BRAS  RE,GETEL            READ FIRST/NEXT FILENAME ELEMENT             
         J     *+8                                                              
TCGFN10  BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
         CLC   CURRDAT8+2(6),TACMCOMM+FILEYEAR-FILENAM                          
         JNE   TCGFN10                                                          
         MVC   FILENAM(32),TACMCOMM                                             
         MVI   TACMEL,X'FF'        MARK ELEMENT AS ALREADY PROCESSED            
         J     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OPEN FILE FOR INPUT                               *         
***********************************************************************         
                                                                                
TCOPENIN NTR1                                                                   
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'WEBFIL'),(X'40',FILENAM)               
         OC    12(4,R1),12(R1)     IF DATASET IS FOUND                          
         JNZ   NO                                                               
         OPEN  (INPFILE,(INPUT))   OPEN FILE FOR INPUT                          
         OI    FILESTAT,FILEIOP                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO CLOSE FILE FOR INPUT                              *         
***********************************************************************         
                                                                                
TCCLOSIN NTR1                                                                   
         CLOSE INPFILE             CLOSE FILE FOR INPUT                         
         NI    FILESTAT,X'FF'-FILEIOP                                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OPEN MQ                                           *         
***********************************************************************         
                                                                                
TCOPENMQ NTR1                                                                   
         TM    MQSTATUS,MQOPEN     EXIT IF MQ IS CURRENTLY OPEN                 
         JO    XIT                                                              
                                                                                
         MVC   TGNAME(16),=C'TALHOLD*********'                                  
         CLC   TGUSER,=H'2276'                                                  
         JNE   *+10                                                             
         MVC   TGNAME(3),=C'TST'                                                
         CLC   TGUSER,=H'7538'                                                  
         JNE   *+10                                                             
         MVC   TGNAME(3),=C'FQA'                                                
                                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,TGNAME),(X'C0',0),0                  
                                                                                
         MVC   TCAUTREA,TCOPFAIL                                                
         CLI   DMCB+8,0            IF MQ OPEN FAILS                             
         JNE   TCEREND             SEND EMAIL NOTIFICATION AND DUMP             
                                                                                
         OI    MQSTATUS,MQOPEN                                                  
                                                                                
         USING FILDATAD,R2                                                      
         LA    R2,TCOPMSG          PUT OPENING MESSAGE TO MQ                    
         LHI   RF,TCOMGLNQ                                                      
         EDIT  (RF),FILLEN                                                      
         MVC   TCOMSGMO,CURRDAT8+4                                              
         MVC   TCOMSGDY,CURRDAT8+6                                              
         MVC   TCOMSGYR,CURRDAT8                                                
         BAS   RE,TCPUTMQ                                                       
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SEND HOLDING FEE ID TO MQ                         *         
*        ON ENTRY ... R2=A(FILDATA)                                   *         
***********************************************************************         
                                                                                
         USING FILDATAD,R2                                                      
TCID2MQ  NTR1                                                                   
         CLI   FILENTRY,DOCID      ONLY WANT TO SEND DOCUMENT IDS               
         JNE   XIT                 TO MQ                                        
         LA    R5,TCIDMSG          PUT MESSAGE TO MQ                            
         LHI   RF,TCIMLNQ                                                       
         MVC   TCIMSGID,FILENTRY+1                                              
         LR    R2,R5                                                            
         EDIT  (RF),FILLEN                                                      
         BAS   RE,TCOPENMQ         ENSURE THAT MQ IS OPEN                       
         BAS   RE,TCPUTMQ          PUT ENTRY TO MQ                              
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO PUT ENTRY TO MQ                                   *         
*        ON ENTRY ... R2=A(FILDATA)                                   *         
***********************************************************************         
                                                                                
         USING FILDATAD,R2                                                      
TCPUTMQ  NTR1                                                                   
         XR    R3,R3               BUILD ENTRY LENGTH IN R3                     
                                                                                
         CLI   FILLEN,C' '                                                      
         JE    TCPMQ10                                                          
         ZIC   R3,FILLEN                                                        
         SHI   R3,X'F0'            MULTIPLY FIRST CHARACTER                     
         MHI   R3,100              OF LENGTH BY 100                             
                                                                                
TCPMQ10  CLI   FILLEN+1,C' '                                                    
         JE    TCPMQ20                                                          
         ZIC   RE,FILLEN+1                                                      
         SHI   RE,X'F0'            MULTIPLY SECOND CHARACTER                    
         MHI   RE,10               OF LENGTH BY 10                              
         AR    R3,RE               AND ADD TO ENTRY LENGTH                      
                                                                                
TCPMQ20  ZIC   RE,FILLEN+2         MULTIPLY THIRD CHARACTER                     
         SHI   RE,X'F0'            OF LENGTH BY 1                               
         AR    R3,RE               AND ADD TO ENTRY LENGTH                      
                                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),FILENTRY,(R3),0                          
                                                                                
         MVC   TCAUTREA,TCPTFAIL                                                
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         JNE   TCEREND             SEND EMAIL NOTIFICATION AND DUMP             
                                                                                
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO CLOSE MQ                                                    
***********************************************************************         
                                                                                
TCCLOMQ  NTR1                                                                   
         TM    MQSTATUS,MQOPEN     IF MQ IS NOT OPEN                            
         JO    TCCMQ10                                                          
         GOTO1 DATVAL,DMCB,CURRDAT8,DUB                                         
         GOTO1 DATCON,DMCB,(0,DUB),(20,NOFILEST)                                
         BAS   RE,TCOPENMQ         OPEN MQ NOW                                  
                                                                                
         USING FILDATAD,R2                                                      
TCCMQ10  LA    R2,TCCLOMSG         PUT CLOSING MESSAGE TO MQ                    
         LHI   RF,TCCMGLNQ                                                      
         EDIT  (RF),FILLEN                                                      
         BRAS  RE,TCPUTMQ                                                       
         DROP  R2                                                               
                                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
                                                                                
         MVC   TCAUTREA,TCCLFAIL  IF MQ CLOSE FAILS                             
         CLI   DMCB+8,0           SEND EMAIL NOTIFICATION AND DUMP              
         JNE   TCEREND                                                          
                                                                                
         NI    MQSTATUS,X'FF'-MQOPEN                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO SEND OUT FAILURE EMAIL AND DUMP                             
***********************************************************************         
                                                                                
TCEREND  GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(TCAUTLEN),TCANOTE)                  
         DC    H'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLES, CONSTANTS, ETC.                                                
***********************************************************************         
                                                                                
INPFILE  DCB   DDNAME=WEBFIL,DSORG=PS,MACRF=(GM),EODAD=TC60,           X        
               RECFM=FB,LRECL=500                                               
                                                                                
MQSCLOS  DC    C'MQ SUCCESSFULLY CLOSED'                                        
                                                                                
DOCID    EQU   C'D'                                                             
                                                                                
TCOPMSG  DS     X                ENTRY STATUS                                   
         DS    XL3               ENTRY LENGTH                                   
TCOMSG   DC    C'<dailyID date="'                                               
TCOMSGMO DS    XL2                                                              
         DC    C'-'                                                             
TCOMSGDY DS    XL2                                                              
         DC    C'-'                                                             
TCOMSGYR DS    XL4                                                              
         DC    C'">'                                                            
TCOMGLNQ EQU   *-TCOMSG                                                         
                                                                                
TCIDMSG  DS    X                 ENTRY STATUS                                   
         DS    XL3               ENTRY LENGTH                                   
TCIMSG   DC    C'<docID value="'                                                
TCIMSGID DS    XL18                                                             
         DC    C'"/>'                                                           
TCIMLNQ  EQU   *-TCIMSG                                                         
                                                                                
TCCLOMSG DS    X                 ENTRY STATUS                                   
         DS    XL3               ENTRY LENGTH                                   
TCCMG    DC    C'</dailyID>'                                                    
TCCMGLNQ EQU   *-TCCMG                                                          
                                                                                
TCANOTE  DC    C'AUTONOTE*US-TALENT_TEAM:'                                      
TCAUTREA DS    CL15                                                             
TCAUTLEN EQU   *-TCANOTE                                                        
TCOPFAIL DC    CL(L'TCAUTREA)'MQ OPEN FAILED'                                   
TCPTFAIL DC    CL(L'TCAUTREA)'MQ PUT ERROR'                                     
TCCLFAIL DC    CL(L'TCAUTREA)'MQ CLOSE FAILED'                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SUMMARIZE TRANSACTIONS FROM CERNO                 *         
***********************************************************************         
                                                                                
         USING T703FFD,RA                                                       
FRCERNO  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,FCINIT         INITIALIZE                                     
                                                                                
         LA    R3,KEY            R3=A(KEY)                                      
                                                                                
         USING TLWTD,R3                                                         
FC10     XC    KEY,KEY           INITIALIZE WEB TRANSACTION KEY                 
         MVI   TLWTCD,TLWTCDQ    TO READ SINCE LAST RUN DATE                    
         MVI   TLWTWBAP,TLWTWACO                                                
         MVC   TLWTDATE,CURRDAT1                                                
         GOTO1 HIGH              READ ALL WEB TRANSACTION KEYS                  
         J     FC30              FOR THE SPECIFIED DATE                         
FC20     GOTO1 SEQ                                                              
FC30     CLC   KEY(TLWTTIME-TLWTD),KEYSAVE                                      
         JNE   FC40                                                             
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC            GET WEB TRANSACTION RECORD                     
                                                                                
         USING TLWTD,R4                                                         
         L     R4,AIO            R4=A(RECORD)                                   
         MVC   CURRDAT1,TLWTDATE SAVE THE CURRENT DATE                          
         MVC   SVWTACTN,TLWTACTN AND ACTION                                     
                                                                                
         BAS   RE,FCOPENMQ                                                      
         GOTO1 FCPUTMQ,DMCB,FCNOT,FCNOTLNQ                                      
                                                                                
         MVC   FCIMSGID,TLWTWBID                                                
         GOTO1 FCPUTMQ,DMCB,FCIMSG,FCIMLNQ                                      
         DROP  R4                                                               
                                                                                
         USING TAWTD,R4                                                         
         MVI   ELCODE,TAWTELQ    GET WEB TRANSACTION ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   FCSTSTAF,TAWTSTAF                                                
                                                                                
         BAS   RE,FCHLD          PUT HOLD DETAILS TO MQ                         
         BAS   RE,FCREL          PUT RELEASE DETAILS TO MQ                      
         BAS   RE,FCLSV          PUT LAST SERVICES DETAILS TO MQ                
         DROP  R4                                                               
                                                                                
         GOTO1 FCPUTMQ,DMCB,FCNOX,FCNOXLNQ                                      
         J     FC20                                                             
                                                                                
FC40     BAS   RE,FCCLOMQ                                                       
                                                                                
         CLC   TGTODAY1,CURRDAT1                                                
         JNH   XIT                                                              
         GOTO1 DATCON,DMCB,(1,CURRDAT1),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK,1                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,CURRDAT1)                                
         J     FC10                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE VARIABLES, RECORDS AND TABLES          *         
***********************************************************************         
                                                                                
FCINIT   NTR1                                                                   
         XC    RECD(RECDLNQ),RECD  AND SETUP LOCAL VARIABLES                    
         BRAS  RE,SETFDAT          SET FIRST DATE TO READ FOR                   
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OPEN MQ                                           *         
*        ON ENTRY ... R4=A(WEB TRANSACTION RECORD)                              
***********************************************************************         
                                                                                
         USING TLWTD,R4                                                         
FCOPENMQ NTR1                                                                   
         TM    MQSTATUS,MQOPEN     EXIT IF MQ IS CURRENTLY OPEN                 
         JO    XIT                                                              
                                                                                
         MVC   TGNAME(16),=C'TALHOLD*********'                                  
         CLC   TGUSER,=H'2276'                                                  
         JNE   *+10                                                             
         MVC   TGNAME(3),=C'TST'                                                
         CLC   TGUSER,=H'7538'                                                  
         JNE   *+10                                                             
         MVC   TGNAME(3),=C'FQA'                                                
                                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,TGNAME),(X'C0',0),0                  
                                                                                
         MVC   FCAUTREA,FCOPFAIL                                                
         CLI   DMCB+8,0            IF MQ OPEN FAILS                             
         JNE   FCEREND             SEND EMAIL NOTIFICATION AND DUMP             
                                                                                
         OI    MQSTATUS,MQOPEN                                                  
                                                                                
         GOTO1 DATCON,DMCB,(1,CURRDAT1),(20,WORK)                               
                                                                                
         MVC   FCOMSGMO,WORK+4                                                  
         MVC   FCOMSGDY,WORK+6                                                  
         MVC   FCOMSGYR,WORK+2                                                  
         GOTO1 FCPUTMQ,DMCB,FCOMSG,FCOMGLNQ                                     
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SEND CERNO HOLD INFORMATION TO MQ                 *         
*        ON ENTRY ... R4=A(WEB TRANSACTION ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING TAWTD,R4                                                         
FCHLD    NTR1                                                                   
         CLI   SVWTACTN,TLWTAHLD                                                
         JNE   XIT                                                              
                                                                                
         GOTO1 FCPUTMQ,DMCB,FCTHMSG,FCTHLNQ                                     
         GOTO1 FCPUTMQ,DMCB,FCSTMSG,FCSTLNQ                                     
                                                                                
         MVC   FCININV,TAWTHCIN                                                 
         GOTO1 FCPUTMQ,DMCB,FCINMSG,FCINLNQ                                     
                                                                                
         MVC   FCDODOL,TAWTHCDO                                                 
         GOTO1 FCPUTMQ,DMCB,FCDOMSG,FCDOLNQ                                     
         DROP  R4                                                               
                                                                                
         GOTO1 FCPUTMQ,DMCB,FCCSMSGX,FCCSXLNQ                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO SEND CERNO RELEASE INFORMATION TO MQ              *         
***********************************************************************         
                                                                                
FCREL    NTR1                                                                   
         CLI   SVWTACTN,TLWTARCO                                                
         JNE   XIT                                                              
                                                                                
         GOTO1 FCPUTMQ,DMCB,FCTRMSG,FCTRLNQ                                     
         GOTO1 FCPUTMQ,DMCB,FCSTMSG,FCSTLNQ                                     
         GOTO1 FCPUTMQ,DMCB,FCINMSGX,FCINXLNQ                                   
         GOTO1 FCPUTMQ,DMCB,FCDOMSGX,FCDOXLNQ                                   
         GOTO1 FCPUTMQ,DMCB,FCCSMSGX,FCCSXLNQ                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO SEND CERNO LAST SERVICES INFORMATION TO MQ        *         
*        ON ENTRY ... R4=A(WEB TRANSACTION ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING TAWTD,R4                                                         
FCLSV    NTR1                                                                   
         CLI   SVWTACTN,TLWTARCA                                                
         JNE   XIT                                                              
                                                                                
         GOTO1 FCPUTMQ,DMCB,FCTLMSG,FCTLLNQ                                     
         GOTO1 FCPUTMQ,DMCB,FCSTMSG,FCSTLNQ                                     
         GOTO1 FCPUTMQ,DMCB,FCINMSGX,FCINXLNQ                                   
         GOTO1 FCPUTMQ,DMCB,FCDOMSGX,FCDOXLNQ                                   
                                                                                
FCLSV10  MVC   FCCSSEQ,TAWTRCSQ                                                 
         GOTO1 FCPUTMQ,DMCB,FCCSMSG,FCCSLNQ                                     
                                                                                
         BRAS  RE,NEXTEL                                                        
         JE    FCLSV10                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO PUT ENTRY TO MQ                                   *         
***********************************************************************         
                                                                                
FCPUTMQ  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),(R2),(R3),0                              
                                                                                
         MVC   FCAUTREA,FCPTFAIL                                                
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         JNE   FCEREND             SEND EMAIL NOTIFICATION AND DUMP             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO CLOSE MQ                                                    
***********************************************************************         
                                                                                
FCCLOMQ  NTR1                                                                   
         TM    MQSTATUS,MQOPEN     IF MQ IS NOT CURRENTLY OPEN                  
         JO    FCCMQ10                                                          
         BAS   RE,FCOPENMQ         OPEN MQ NOW                                  
                                                                                
FCCMQ10  GOTO1 FCPUTMQ,DMCB,FCCMG,FCCMGLNQ                                      
                                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
                                                                                
         MVC   FCAUTREA,FCCLFAIL  IF MQ CLOSE FAILS                             
         CLI   DMCB+8,0           SEND EMAIL NOTIFICATION AND DUMP              
         JNE   FCEREND                                                          
                                                                                
         NI    MQSTATUS,X'FF'-MQOPEN                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO SEND OUT FAILURE EMAIL AND DUMP                             
***********************************************************************         
                                                                                
FCEREND  GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(FCAUTLEN),FCANOTE)                  
         DC    H'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLES, CONSTANTS, ETC.                                                
***********************************************************************         
                                                                                
FCOMSG   DC    C'<HoldReleaseDailyReport date="'                                
FCOMSGMO DS    XL2                                                              
         DC    C'-'                                                             
FCOMSGDY DS    XL2                                                              
         DC    C'-20'                                                           
FCOMSGYR DS    XL2                                                              
         DC    C'">'                                                            
FCOMGLNQ EQU   *-FCOMSG                                                         
                                                                                
FCNOT    DC    C'<notice>'                                                      
FCNOTLNQ EQU   *-FCNOT                                                          
                                                                                
FCIMSG   DC    C'<externalDocId>'                                               
         DC    C'HF'                                                            
FCIMSGID DS    XL16                                                             
         DC    C'</externalDocId>'                                              
FCIMLNQ  EQU   *-FCIMSG                                                         
                                                                                
FCTHMSG  DC    C'<transaction>Hold</transaction>'                               
FCTHLNQ  EQU   *-FCTHMSG                                                        
                                                                                
FCTRMSG  DC    C'<transaction>Release</transaction>'                            
FCTRLNQ  EQU   *-FCTRMSG                                                        
                                                                                
FCTLMSG  DC    C'<transaction>LastServiceCast</transaction>'                    
FCTLLNQ  EQU   *-FCTLMSG                                                        
                                                                                
FCSTMSG  DC    C'<staffid>'                                                     
FCSTSTAF DS    XL8                                                              
         DC    C'</staffid>'                                                    
FCSTLNQ  EQU   *-FCSTMSG                                                        
                                                                                
FCINMSG  DC    C'<invoicenumber>'                                               
FCININV  DS    XL6                                                              
         DC    C'</invoicenumber>'                                              
FCINLNQ  EQU   *-FCINMSG                                                        
                                                                                
FCINMSGX DC    C'<invoicenumber/>'                                              
FCINXLNQ EQU   *-FCINMSGX                                                       
                                                                                
FCDOMSG  DC    C'<totaldollars>'                                                
FCDODOL  DS    XL10                                                             
         DC    C'</totaldollars>'                                               
FCDOLNQ  EQU   *-FCDOMSG                                                        
                                                                                
FCDOMSGX DC    C'<totaldollars/>'                                               
FCDOXLNQ EQU   *-FCDOMSGX                                                       
                                                                                
FCCSMSG  DC    C'<castsequence>'                                                
FCCSSEQ  DS    XL4                                                              
         DC    C'</castsequence>'                                               
FCCSLNQ  EQU   *-FCCSMSG                                                        
                                                                                
FCCSMSGX DC    C'<castsequence/>'                                               
FCCSXLNQ EQU   *-FCCSMSGX                                                       
                                                                                
FCNOX    DC    C'</notice>'                                                     
FCNOXLNQ EQU   *-FCNOX                                                          
                                                                                
FCCMG    DC    C'</HoldReleaseDailyReport>'                                     
FCCMGLNQ EQU   *-FCCMG                                                          
                                                                                
FCANOTE  DC    C'AUTONOTE*US-TALENT_TEAM:'                                      
FCAUTREA DS    CL15                                                             
FCAUTLEN EQU   *-FCANOTE                                                        
FCOPFAIL DC    CL(L'TCAUTREA)'MQ OPEN FAILED'                                   
FCPTFAIL DC    CL(L'TCAUTREA)'MQ PUT ERROR'                                     
FCCLFAIL DC    CL(L'TCAUTREA)'MQ CLOSE FAILED'                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SUMMARIZE TRANSACTIONS FROM VITA                  *         
***********************************************************************         
                                                                                
         USING T703FFD,RA                                                       
FRVITA   NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,FVINIT         INITIALIZE                                     
                                                                                
         LA    R3,KEY            R3=A(KEY)                                      
                                                                                
         USING TLWTD,R3                                                         
FV10     XC    KEY,KEY           INITIALIZE WEB TRANSACTION KEY                 
         MVI   TLWTCD,TLWTCDQ    TO READ SINCE LAST RUN DATE                    
         MVI   TLWTWBAP,TLWTWAVI                                                
         MVC   TLWTDATE,CURRDAT1                                                
         GOTO1 HIGH              READ ALL WEB TRANSACTION KEYS                  
         J     FV30              FOR THE SPECIFIED DATE                         
FV20     GOTO1 SEQ                                                              
FV30     CLC   KEY(TLWTTIME-TLWTD),KEYSAVE                                      
         JNE   FV90                                                             
         CLI   TLWTACTN,TLWTAACO                                                
         JL    FV20                                                             
                                                                                
         MVC   CURRDAT1,TLWTDATE SAVE THE CURRENT DATE                          
         MVC   SVWTACTN,TLWTACTN AND ACTION                                     
                                                                                
         GOTO1 DATCON,DMCB,(1,TLWTDATE),(20,WORK)                               
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),8                                      
                                                                                
         MVC   FULL(2),TLWTTIME                                                 
         MVI   FULL+2,0                                                         
         GOTO1 TIMECON,DMCB,FULL,TLWTDATE,(8,WORK)                              
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),5                                      
                                                                                
         GOTO1 (RF),(R1),(C'T',TLWTWBID),L'TLWTWBID                             
         MVC   TGBYTE,TLWTACTN                                                  
                                                                                
         GOTO1 GETREC            GET WEB TRANSACTION RECORD                     
                                                                                
         USING TAWTD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAWTELQ    GET WEB TRANSACTION ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',TAWTSTAF),L'TAWTSTAF                         
                                                                                
         MVC   WORK,ADDCOM                                                      
         CLI   TGBYTE,TLWTAACO                                                  
         JE    FV40                                                             
         MVC   WORK,CHGCOM                                                      
         CLI   TGBYTE,TLWTACCO                                                  
         JE    FV40                                                             
         MVC   WORK,ADDCST                                                      
         CLI   TGBYTE,TLWTAACA                                                  
         JE    FV40                                                             
         MVC   WORK,CHGCST                                                      
         CLI   TGBYTE,TLWTACCA                                                  
         JE    FV40                                                             
         MVC   WORK,DELCST                                                      
         CLI   TGBYTE,TLWTADCA                                                  
         JE    FV40                                                             
         MVC   WORK,ADDVER                                                      
         CLI   TGBYTE,TLWTAAVR                                                  
         JE    FV40                                                             
         MVC   WORK,CHGVER                                                      
         CLI   TGBYTE,TLWTACVR                                                  
         JE    FV40                                                             
         MVC   WORK,DELVER                                                      
         CLI   TGBYTE,TLWTADVR                                                  
         JE    FV40                                                             
         MVC   WORK,ADDNMD                                                      
         CLI   TGBYTE,TLWTAANM                                                  
         JE    FV40                                                             
         MVC   WORK,ADDINT                                                      
         CLI   TGBYTE,TLWTAAIN                                                  
         JE    FV40                                                             
         CLI   TGBYTE,TLWTAPAY                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   WORK,PAY                                                         
         MVC   WORK+6(L'TAWTPYUS),TAWTPYUS                                      
FV40     GOTO1 (RF),(R1),(C'T',WORK),L'ADDCOM                                   
                                                                                
         CLI   TGBYTE,TLWTAANM                                                  
         JE    FV70                                                             
         CLI   TGBYTE,TLWTAAIN                                                  
         JE    FV70                                                             
                                                                                
         GOTO1 (RF),(R1),(C'T',TAWTCOAY),L'TAWTCOAY                             
         GOTO1 (RF),(R1),(C'T',TAWTCOID),L'TAWTCOID                             
                                                                                
         CLI   TGBYTE,TLWTAAVR                                                  
         JNL   FV60                                                             
         GOTO1 (RF),(R1),(C'T',TAWTCOTI),L'TAWTCOTI                             
                                                                                
         CLI   TGBYTE,TLWTAPAY                                                  
         JNE   FV50                                                             
         MVI   BYTE,C' '                                                        
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',TLWTUINV),L'TLWTUINV                             
         GOTO1 (RF),(R1),(C'T',TAWTPYDO),L'TAWTPYDO                             
         J     FV80                                                             
                                                                                
FV50     CLI   TGBYTE,TLWTAACA                                                  
         JL    FV80                                                             
         GOTO1 SSNPACK,DMCB,TAWTCASS,TGPID                                      
         GOTO1 OUTPDOWN,DMCB,(C'T',TGPID),L'TGPID                               
         GOTO1 SQUASHER,DMCB,TAWTCANM,L'TAWTCANM                                
         GOTO1 OUTPDOWN,DMCB,(C'T',TAWTCANM),L'TAWTCANM                         
         GOTO1 (RF),(R1),(C'T',TAWTCACA),L'TAWTCACA                             
         J     FV80                                                             
                                                                                
FV60     MVI   BYTE,C' '                                                        
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         EDIT  TLWTUVER,TGTHREE,ALIGN=LEFT                                      
         GOTO1 OUTPDOWN,DMCB,(C'T',TGTHREE),L'TGTHREE                           
         GOTO1 (RF),(R1),(C'T',TAWTVRID),L'TAWTVRID                             
         GOTO1 (RF),(R1),(C'T',TAWTVRTI),L'TAWTVRTI                             
         J     FV80                                                             
                                                                                
FV70     MVI   BYTE,C' '                                                        
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',TAWTNINM),L'TAWTNINM                             
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',BYTE),L'BYTE                                     
         GOTO1 (RF),(R1),(C'T',TLWTUNIC),L'TLWTUNIC                             
         DROP  R3,R4                                                            
                                                                                
FV80     BAS   RE,EOLDOWN                                                       
         J     FV20                                                             
                                                                                
FV90     CLC   TGTODAY1,CURRDAT1                                                
         JNH   FV100                                                            
         GOTO1 DATCON,DMCB,(1,CURRDAT1),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK,1                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,CURRDAT1)                                
         J     FV10                                                             
                                                                                
FV100    BAS   RE,ENDDOWN                                                       
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE VARIABLES, RECORDS AND TABLES          *         
***********************************************************************         
                                                                                
FVINIT   NTR1                                                                   
         XC    RECD(RECDLNQ),RECD  AND SETUP LOCAL VARIABLES                    
         BRAS  RE,SETFDAT          SET FIRST DATE TO READ FOR                   
                                                                                
         BAS   RE,INITDOWN                                                      
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',DATHEAD),L'DATHEAD                           
         GOTO1 (RF),(R1),(C'T',TIMHEAD),L'TIMHEAD                               
         GOTO1 (RF),(R1),(C'T',SESHEAD),L'SESHEAD                               
         GOTO1 (RF),(R1),(C'T',STFHEAD),L'STFHEAD                               
         GOTO1 (RF),(R1),(C'T',ACTHEAD),L'ACTHEAD                               
         GOTO1 (RF),(R1),(C'T',AGYHEAD),L'AGYHEAD                               
         GOTO1 (RF),(R1),(C'T',CIDHEAD),L'CIDHEAD                               
         GOTO1 (RF),(R1),(C'T',TITHEAD),L'TITHEAD                               
         GOTO1 (RF),(R1),(C'T',PIDHEAD),L'PIDHEAD                               
         GOTO1 (RF),(R1),(C'T',NAMHEAD),L'NAMHEAD                               
         GOTO1 (RF),(R1),(C'T',CATHEAD),L'CATHEAD                               
******** GOTO1 (RF),(R1),(C'T',VERHEAD),L'VERHEAD                               
******** GOTO1 (RF),(R1),(C'T',VIDHEAD),L'VIDHEAD                               
******** GOTO1 (RF),(R1),(C'T',VTIHEAD),L'VTIHEAD                               
******** GOTO1 (RF),(R1),(C'T',CODHEAD),L'CODHEAD                               
         BAS   RE,EOLDOWN                                                       
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
         USING DLCBD,R5                                                         
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SET FIRST DATE TO READ FOR                        *         
***********************************************************************         
                                                                                
SETFDAT  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)                                    
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO              GET LAST RUN DATE FROM SYSTEM                
         MVI   ELCODE,TASYELQ      RECORD                                       
         BRAS  RE,GETEL            AND ADD ONE DAY TO IT                        
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTO1 DATCON,DMCB,(1,TASYLWSU),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK,1                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,CURRDAT1)                                
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO UPDATE COMMERCIAL CLIENT GROUPS                   *         
***********************************************************************         
                                                                                
UPDCGS   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,KEY            R3=A(KEY)                                      
         BRAS  RE,SETFDAT        SET FIRST DATE TO READ FOR                     
                                                                                
UCGS10   XC    SVWTKEY,SVWTKEY                                                  
                                                                                
         USING TLWTD,R3                                                         
         XC    KEY,KEY           INITIALIZE TRANSACTION KEY                     
         MVI   TLWTCD,TLWTCDQ    TO READ SINCE LAST RUN DATE                    
         MVI   TLWTWBAP,TLWTMFCG                                                
         MVC   TLWTDATE,CURRDAT1                                                
         GOTO1 HIGH              READ ALL WEB TRANSACTION KEYS                  
         J     UCGS30            FOR THE SPECIFIED DATE                         
UCGS20   GOTO1 SEQ                                                              
UCGS30   CLC   KEY(TLWTTIME-TLWTD),KEYSAVE                                      
         JNE   UCGS90                                                           
         CLC   TLWTCGAY,SVWTKEY+TLWTCGAY-TLWTD                                  
         JNE   UCGS40                                                           
         CLC   TLWTCGCL,SVWTKEY+TLWTCGCL-TLWTD                                  
         JE    UCGS20                                                           
                                                                                
UCGS40   MVC   SVWTKEY,KEY                                                      
         DROP  R3                                                               
                                                                                
         USING TLCOD,R3                                                         
         XC    TLCOKEY,TLCOKEY     READ ALL OF THIS CLIENT'S                    
         MVI   TLCOCD,TLCOCDQ      COMMERCIALS                                  
         MVC   TLCOAGY,SVWTKEY+TLWTCGAY-TLWTD                                   
         MVC   TLCOCLI,SVWTKEY+TLWTCGCL-TLWTD                                   
         GOTO1 HIGH                                                             
         J     UCGS60                                                           
UCGS50   GOTO1 SEQ                                                              
UCGS60   CLC   KEY(TLCOPRD-TLCOD),KEYSAVE                                       
         JNE   UCGS80                                                           
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL            AND UPDATE CLIENT GROUP                      
         JNE   UCGS70                                                           
                                                                                
         MVC   SVCLKEY,KEY                                                      
                                                                                
         CLC   TACOCLG,SVWTKEY+TLWTCGCG-TLWTD                                   
         JE    UCGS70                                                           
                                                                                
         GOTO1 ASAVPTRS,DMCB,ASVPTRS                                            
                                                                                
         MVC   TACOCLG,SVWTKEY+TLWTCGCG-TLWTD                                   
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
                                                                                
         GOTO1 AADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                
                                                                                
         USING TLCOD,R3                                                         
         MVC   KEY,SVCLKEY                                                      
UCGS70   ZICM  RE,TLCOCOM,4                                                     
         AHI   RE,1                                                             
         STCM  RE,15,TLCOCOM                                                    
         GOTO1 HIGH                                                             
         J     UCGS60                                                           
         DROP  R3                                                               
                                                                                
UCGS80   MVC   KEY,SVWTKEY                                                      
         GOTO1 HIGH                                                             
         J     UCGS20                                                           
                                                                                
UCGS90   CLC   TGTODAY1,CURRDAT1                                                
         JNH   XIT                                                              
         GOTO1 DATCON,DMCB,(1,CURRDAT1),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK,1                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,CURRDAT1)                                
         J     UCGS10                                                           
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO UPDATE LAST RUN DATE ON SYSTEM RECORD             *         
***********************************************************************         
                                                                                
UPDLDAT  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0)                                    
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVSYLWSU,TASYLWSU                                                
         MVC   TASYLWSU,TGTODAY1                                                
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC                                                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CLEAR DAY'S FILENNAMES FROM WEB FILENAMES RECORD  *         
***********************************************************************         
                                                                                
TCCFILEN NTR1  BASE=*,LABEL=*                                                   
         USING TLSYD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY             GET ALL HOLDING FEE WEB FILENAME             
         MVI   TLSYCD,TLSYCDQ      RECORDS                                      
         MVI   TLSYTYPE,TLSYHFFN                                                
         GOTO1 HIGH                                                             
         J     TCCFN20                                                          
TCCFN10  GOTO1 SEQ                                                              
TCCFN20  CLC   TLSYKEY,KEYSAVE                                                  
         JNE   XIT                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,TACMELQ      DELETE ALL FILENAME ELEMENTS                 
         GOTO1 REMELEM                                                          
         GOTO1 PUTREC              AND PUT BACK RECORD                          
         J     TCCFN10                                                          
         EJECT                                                                  
***********************************************************************         
*        TABLES, CONSTANTS, ETC.                                                
***********************************************************************         
                                                                                
DATHEAD  DC    C'DATE'                                                          
TIMHEAD  DC    C'TIME'                                                          
SESHEAD  DC    C'SESSION ID'                                                    
STFHEAD  DC    C'STAFF'                                                         
ACTHEAD  DC    C'ACTION'                                                        
AGYHEAD  DC    C'AGENCY'                                                        
CIDHEAD  DC    C'COMMERCIAL ID'                                                 
TITHEAD  DC    C'TITLE'                                                         
PIDHEAD  DC    C'PID'                                                           
NAMHEAD  DC    C'NAME'                                                          
CATHEAD  DC    C'CATEGORY'                                                      
*INVHEAD DC    C'INVOICE'                                                       
*DOLHEAD DC    C'GROSS'                                                         
VERHEAD  DC    C'VERSION'                                                       
VIDHEAD  DC    C'VERSION ID'                                                    
VTIHEAD  DC    C'VERSION TITLE'                                                 
CODHEAD  DC    C'CODE'                                                          
                                                                                
ADDCOM   DC    C'ADD COMMERCIAL   '                                             
CHGCOM   DC    C'CHANGE COMMERCIAL'                                             
ADDCST   DC    C'ADD CAST         '                                             
CHGCST   DC    C'CHANGE CAST      '                                             
PAY      DC    C'PAY -            '                                             
DELCST   DC    C'DELETE CAST      '                                             
ADDVER   DC    C'ADD VERSION      '                                             
CHGVER   DC    C'CHANGE VERSION   '                                             
DELVER   DC    C'DELETE VERSION   '                                             
ADDNMD   DC    C'ADD NEW MEDIA    '                                             
ADDINT   DC    C'ADD INTERNET     '                                             
                                                                                
ASVPTRS  DS    A                 A(SAVED POINTER BLOCK)                         
AUPPTRS  DS    A                 A(UPDATED POINTER BLOCK)                       
                                                                                
         DS    50X                                                              
         LTORG                                                                  
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
                                                                                
RECD     DSECT                                                                  
SVSYLWSU DS    XL3               LAST RUN DATE                                  
CURRDAT1 DS    XL3               FIRST DATE                                     
CURRDAT8 DS    XL8               CURRENT DATE                                   
                                                                                
SVWTACTN DS    XL(L'TLWTACTN)    SAVED ACTION                                   
SVWTKEY  DS    XL(L'KEY)         SAVED TRANSACTION KEY                          
SVCLKEY  DS    XL(L'KEY)         SAVED CLIENT KEY                               
                                                                                
FILENAM  DS    CL34                                                             
DYNALLOC DS    A                 A(DYNAMIC ALLOCATION ROUTINE)                  
                                                                                
         ORG   FILENAM                                                          
         DS    CL7               TALDISK                                        
         DS    CL1               .                                              
         DS    CL4               TAOW                                           
FILESYS  DS    CL1               ADV(1) OR TEST(2)                              
FILEREP  DS    CL2               HF,SN,HU OR HB                                 
FILEWHEN DS    CL1               OVERNIGHT OR SOON                              
NOFILEST DS    CL1               .                                              
         DS    CL1               D                                              
FILEYEAR DS    CL2               YEAR                                           
FILEMON  DS    CL2               MONTH                                          
FILEDAY  DS    CL2               DAY                                            
FDATELNQ EQU   *-FILEYEAR                                                       
         DS    CL1               .                                              
         DS    CL1               T                                              
FILEHOUR DS    CL2               HOUR                                           
FILEMIN  DS    CL2               MINUTE                                         
FILESEC  DS    CL2               SECONDS                                        
         DS    CL2                                                              
                                                                                
MQSTATUS DS    X                 CURRENT MQ STATUS                              
MQOPEN   EQU   X'80'             MQ HAS BEEN OPENED                             
                                                                                
FILESTAT DS    X                 CURRENT FILE STATUS                            
FILEIOP  EQU   X'80'             FILE OPEN FOR INPUT                            
                                                                                
DATETAB  DS    XL((DTLNQ*10)+1)                                                 
RECDLNQ  EQU   *-RECD                                                           
                                                                                
DLCB    DS     CL(DLCBXLX)       DOWNLOAD BLOCK                                 
        ORG    DLCB                                                             
FILDATA DS     XL500             ENTRY READ FROM FILE                           
         EJECT                                                                  
*              DSECT TO COVER ENTRIES FROM DATE TABLE                           
                                                                                
DATETABD DSECT                                                                  
DTYEAR   DS    CL2               ENTRY YEAR                                     
DTMON    DS    CL2               ENTRY MONTH                                    
DTDAY    DS    CL2               ENTRY DAY                                      
DTLNQ    EQU   *-DATETABD                                                       
         EJECT                                                                  
*              DSECT TO COVER ENTRY READ FROM XML FILE                          
                                                                                
FILDATAD DSECT                                                                  
FILSTAT  DS    X                 ENTRY STATUS                                   
FILSUMQ  EQU   C'X'              ENTRY OK FOR SUMMARY                           
FILLEN   DS    XL3               ENTRY LENGTH                                   
FILENTRY DS    0X                ENTRY DATA                                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDTWADCOND                                                                    
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* DDMASTD                                                                       
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR NMOD STORAGE GRAB AREA                            *          
**********************************************************************          
                                                                                
TMPD     DSECT                                                                  
SVPTRBLK DS    CL((520*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS                      
UPPTRBLK DS    CL((520*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS FOR ADDPTRS          
TMPLNQ   EQU   *-TMPD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP5B   10/23/14'                                      
         END                                                                    
