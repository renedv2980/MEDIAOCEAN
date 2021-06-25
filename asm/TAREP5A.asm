*          DATA SET TAREP5A    AT LEVEL 002 AS OF 01/19/06                      
*PHASE T7035AA                                                                  
         TITLE 'T7035A - SEND XML FILE TO MQ'                                   
T7035A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7035A                                                         
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
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF CALLED WITH PRINTREP                      
         BNE   XIT                                                              
         SPACE 1                                                                
         USING SOFTVARD,R4                                                      
         BAS   RE,INIT             INITIALIZE VARIABLES                         
         SPACE 1                                                                
         BAS   RE,CNTMSGS          COUNT MESSAGES IN XML FILE                   
         BAS   RE,MKNOSND          MARK ENTS THAT SHOULD NOT GO TO MQ           
         SPACE 1                                                                
         BAS   RE,OPENINP          OPEN XML FILE FOR INPUT                      
         SPACE 1                                                                
         USING FILDATAD,R2                                                      
         LA    R2,FILDATA                                                       
F2M10    GET   INPFILE,FILDATA     GET ENTRY FROM INPUT FILE                    
         SPACE 1                                                                
         CLI   FILSTAT,FILSTMQ     IF ENTRY SHOULD NOT BE SENT TO MQ            
         BE    F2M10               DO NOT SEND IT                               
         BAS   RE,OPENMQ           ENSURE THAT MQ IS OPEN                       
         BAS   RE,PUTMQ            PUT ENTRY TO MQ                              
         B     F2M10               GO GET NEXT ENTRY                            
         DROP  R2                                                               
         SPACE 1                                                                
F2M20    BAS   RE,CLOSEMQ          WHEN ALL ENTRIES PROCESSED                   
         BAS   RE,CLOSEBK          CLOSE MQ, BACKUP FILE                        
         BAS   RE,CLOSINP          AND INPUT FILE                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INITIALIZE VARIABLES                                  
*              ON EXIT ... R4=A(SOFT VARABLE TABLE ENTRY)                       
         SPACE 1                                                                
INIT     NTR1                                                                   
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         XC    RECD(RECDLNQ),RECD  AND CLEAR LOCAL VARIABLES                    
         SPACE 1                                                                
         USING MASTD,RE                                                         
         L     RE,TWAMASTC                                                      
         MVC   PERCHUNK,MCCOMMIT   NUMBER OF RECORDS PER MQ CHUNK               
         DROP  RE                                                               
         SPACE 1                                                                
         LA    R4,SOFTVARS         MATCH UP CURRENT REPORT NUMBER               
INIT10   CLI   0(R4),X'FF'         TO SOFT VARIABLE TABLE'S ENTRY               
         BNE   *+6                 FOR THIS REPORT NUMBER                       
         DC    H'00'                                                            
         MVC   OM2SOFT,SVPTOPLB                                                 
         CLC   RECNUM,0(R4)        RETURN ADDRESS OF ENTRY IN R4                
         BE    XIT                                                              
         LA    R4,SVLNQ(R4)                                                     
         B     INIT10                                                           
         EJECT                                                                  
*              ROUTINE TO COUNT NUMBER OF "MQ SUCCESSFULLY CLOSED"              
*              AND NUMBER OF "ALL PREVIOUS PUTS COMMITTED" IN FILE              
         SPACE 1                                                                
CNTMSGS  NTR1                                                                   
         XR    R5,R5               R5=MQ SUCCESSFULLY CLOSED COUNTER            
         XR    R6,R6               R6=ALL PREV PUTS COMMITTED COUNTER           
         SPACE 1                                                                
         BAS   RE,OPENCNT          OPEN XML FILE FOR COUNTING                   
         SPACE 1                                                                
         USING FILDATAD,R2                                                      
         LA    R2,FILDATA                                                       
CMSGS10  GET   CNTFILE,FILDATA     GET ENTRY FROM INPUT FILE                    
         SPACE 1                                                                
         CLC   MQSCLOS,FILENTRY    IF ENTRY IS "MQ SUCCESSFULLY CLOSED"         
         BNE   CMSGS20             INCREASE ITS COUNTER                         
         AHI   R5,1                                                             
         SPACE 1                                                                
CMSGS20  CLC   PPUTCOM,FILENTRY    IF ENTRY IS "ALL PREV PUTS COMM'             
         BNE   CMSGS10             INCREASE ITS COUNTER                         
         AHI   R6,1                                                             
         B     CMSGS10                                                          
         SPACE 1                                                                
CMSGS30  LTR   R6,R6               IF NO RECORDS WERE EVER COMMITTED            
         BNZ   CMSGS40             NO REASON TO RUN THIS PROGRAM                
         DC    H'00'                                                            
         SPACE 1                                                                
CMSGS40  ST    R5,CLCOUNT          SAVE COUNTERS                                
         ST    R6,CMCOUNT                                                       
         BAS   RE,CLOSCNT          AND CLOSE FILE FOR COUNTING                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MARK OFF ENTRIES ALREADY SENT TO MQ                   
*              AND ENTRIES THAT WERE NOT COMMITTED AS "DO NOT                   
*              SEND TO MQ"                                                      
MKNOSND  NTR1                                                                   
         L     R5,CLCOUNT          R5=NUMBER MQ CLOSES ALREADY ISSUED           
         SPACE 1                                                                
         LTR   R5,R5               IF NO MQ CLOSES ALREADY ISSUED, NO           
         BZ    MPREVX              ENTRIES TO MARK OFF AS DO NOT SEND           
         SPACE 1                                                                
         USING FILDATAD,R2                                                      
         BAS   RE,OPENUPD          OPEN FILE FOR UPDATE                         
MPREV10  GET   UPDFILE             GET XML FILE ENTRY                           
         LR    R2,R1                                                            
         MVI   FILSTAT,FILSTMQ     MARK IT AS DO NOT SEND TO MQ                 
         PUTX  UPDFILE             AND PUT IT BACK TO FILE                      
         CLC   MQSCLOS,FILENTRY    IF ENTRY IS "MQ SUCCESSFULLY CLOSED"         
         BNE   MPREV10             DECREASE COUNTER OF MQ CLOSED'S              
         BCT   R5,MPREV10                                                       
         BAS   RE,CLOSUPD          CLOSE THE FILE FOR UPDATE                    
         SPACE 1                                                                
         L     R6,CMCOUNT          R6=NUMBER OF COMMITS ISSUED                  
         SPACE 1                                                                
         BAS   RE,OPENUPD          OPEN FILE FOR UPDATE                         
         OI    FILESTAT,FILEACM    TO MARK OFF POST-COMMIT ENTRIES              
MPREV20  GET   UPDFILE             GET XML FILE ENTRY                           
         LR    R2,R1                                                            
         CLC   PPUTCOM,FILENTRY    BUMP TO THE LAST SUCCESSFUL COMMIT           
         BNE   MPREV20                                                          
         BCT   R6,MPREV20                                                       
MPREV30  GET   UPDFILE             ALL ENTRIES AFTER LAST SUCCESSFUL            
         LR    R2,R1               COMMIT                                       
         MVI   FILSTAT,FILSTMQ     DO NOT GET SENT TO MQ AGAIN                  
         PUTX  UPDFILE                                                          
         B     MPREV30                                                          
MPREV40  TM    FILESTAT,FILEACM    SHOULD ONLY REACH END OF UPDATE              
         BO    *+6                 FILE WHEN MARKING OFF ENTRIES                
         DC    H'00'               AFTER LAST SUCCESSFUL COMMIT                 
         BAS   RE,CLOSUPD          CLOSE THE FILE FOR UPDATE                    
         NI    FILESTAT,X'FF'-FILEACM                                           
         DROP  R2                                                               
MPREVX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OPEN FILE FOR COUNTING                                
         SPACE 1                                                                
OPENCNT  NTR1                                                                   
         OPEN  (CNTFILE,(INPUT))   OPEN FILE FOR COUNTING                       
         OI    FILESTAT,FILECOP                                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO OPEN FILE FOR INPUT                                   
         SPACE 1                                                                
OPENINP  NTR1                                                                   
         OPEN  (INPFILE,(INPUT))   OPEN FILE FOR INPUT                          
         OI    FILESTAT,FILEIOP                                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO OPEN FILE FOR UPDATE                                  
         SPACE 1                                                                
OPENUPD  NTR1                                                                   
         OPEN  (UPDFILE,(UPDAT))   OPEN FILE FOR UPDATE                         
         OI    FILESTAT,FILEUOP                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OPEN MQ                                               
         SPACE 1                                                                
OPENMQ   NTR1                                                                   
         TM    MQSTATUS,MQOPEN     EXIT IF MQ ALREADY OPENED                    
         BO    XIT                                                              
         SPACE 1                                                                
         BAS   RE,OPENBK           MAY NEED TO OPEN BACKUP FILE                 
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,SVMQOPLB),(X'C0',0),0                
         SPACE 1                                                                
         MVC   AUTOREAS,OPENFAIL                                                
         CLI   DMCB+8,0            IF MQ OPEN FAILS                             
         BNE   ERREND              SEND EMAIL NOTIFICATION AND DUMP             
         SPACE 1                                                                
         OI    MQSTATUS,MQOPEN     TURN ON MQ OPENED STATUS                     
         MVC   CNTCHUNK,PERCHUNK   SET NUMBER OF RECORDS IN MQ CHUNK            
         SPACE 1                                                                
         USING FILDATAD,R2                                                      
         LA    R2,OPENMSG1         PUT OPENING MESSAGE TO MQ                    
         LHI   RF,OMSG1LNQ                                                      
         EDIT  (RF),FILLEN                                                      
         BAS   RE,PUTMQ                                                         
         SPACE 1                                                                
         LA    R2,OPENMSG2         PUT SECOND OPENING MESSAGE TO MQ             
         LHI   RF,OMSG2LNQ                                                      
         EDIT  (RF),FILLEN                                                      
         BAS   RE,PUTMQ                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO OPEN BACKUP FILE                                      
         SPACE 1                                                                
OPENBK   NTR1                                                                   
         TM    BKSTATUS,BKOPEN     IF BACKUP FILE NOT ALREADY OPENED            
         BO    XIT                                                              
         DROP  RA                                                               
         SPACE 1                                                                
         USING WEBNAMD,R2                                                       
         LA    R2,WORK             BUILD BACKUP FILE NAME IN WORK               
         MVC   WORK,SPACES                                                      
         SPACE 1                                                                
         MVC   WNHEAD,WEBHEAD      SET FILE NAME HEADER                         
         SPACE 1                                                                
         MVI   WNRUNSYS,C'1'       INDICATE TPNY RUN                            
         CLC   AGYALPHA,=C'D2'                                                  
         BNE   OBK10                                                            
         MVI   WNRUNSYS,C'2'       OR TEST SYSTEM RUN                           
         SPACE 1                                                                
OBK10    MVC   WNRUNTYP,SVBKFLLB   INDICATE BACKUP RUN                          
         MVC   WNRUNWHN(L'WEBBK),WEBBK                                          
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,TGTODAY8,DUB                                         
         GOTO1 DATCON,DMCB,(0,DUB),(20,TGDUB)                                   
         MVC   WNRUNDTE,TGDUB+2    INDICATE RUN DATE                            
         SPACE 1                                                                
         MVC   WNDOT3(L'WEBTIME),WEBTIME                                        
         TIME  DEC                 INDICATE RUN TIME                            
         ST    R0,TGDUB                                                         
         GOTO1 HEXOUT,DMCB,TGDUB,WNRUNTIM,3                                     
         DROP  R2                                                               
         SPACE 1                                                                
         USING CONHEADH-64,RE                                                   
         L     RE,ATWA                                                          
         SPACE 1                                                                
         USING TWADCOND,R1                                                      
         L     R1,TWADCONS                                                      
         MVC   DYNALLOC,TDYNALLO                                                
         DROP  R1,RE                                                            
         SPACE 1                                                                
         LA    R2,TGDUB                                                         
         MVC   TGDUB,=X'000012000001'                                           
         GOTO1 DYNALLOC,DMCB,(X'80',=CL8'BAKFIL'),                     +        
               (X'41',(R2)),(X'80',WORK)                                        
         SPACE 1                                                                
         OPEN  (BAKFILE,(OUTPUT))  OPEN BACKUP FILE FOR OUTPUT                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         OI    BKSTATUS,BKOPEN     TURN ON OPENED BACKUP FILE STATUS            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT ENTRY TO MQ                                       
*              ON ENTRY ... R2=A(FILDATA)                                       
         SPACE 1                                                                
         USING FILDATAD,R2                                                      
PUTMQ    NTR1                                                                   
         BAS   RE,BLDLEN           BUILD ENTRY LENGTH IN R3                     
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),FILENTRY,(R3),0                          
         SPACE 1                                                                
         MVC   AUTOREAS,PUTFAIL                                                 
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         BNE   ERREND              SEND EMAIL NOTIFICATION AND DUMP             
         SPACE 1                                                                
         CLC   OMSG1,FILENTRY      IF NOT PUTTING FIRST MQ OPEN MESSAGE         
         BE    PUTMQ20                                                          
         CLC   OMSG2,FILENTRY      OR SECOND MQ OPEN MESSAGE                    
         BE    PUTMQ20                                                          
         CLC   CMSG,FILENTRY       IF PUTTING MQ CLOSE MESSAGE                  
         BE    PUTMQ20             SEND FAKE "ALL PREVIOUS PUTS                 
         L     RF,MQCOUNT          AND MQ PUT IS SUCCESSFUL                     
         AHI   RF,1                ADD ONE TO MQ ENTRIES COUNT                  
         ST    RF,MQCOUNT                                                       
         SPACE 1                                                                
PUTMQ10  BAS   RE,CLRBK            SEND ENTRY TO BACKUP FILE TOO                
         LA    RE,BAKDATA                                                       
         LR    RF,R2                                                            
         MVC   0(BAKLNQ/2,RE),0(RF)                                             
         LA    RE,BAKLNQ/2(RE)                                                  
         LA    RF,BAKLNQ/2(RF)                                                  
         MVC   0(BAKLNQ/2,RE),0(RF)                                             
         BAS   RE,PUTBK                                                         
         SPACE 1                                                                
PUTMQ20  CLC   GROUPEND,FILENTRY   IF AT THE END OF GROUPED ENTRIES             
         BNE   XIT                                                              
         L     R2,BKCOUNT          ADD ONE REC TO BACKUP FILE COUNTER           
         AHI   R2,1                                                             
         ST    R2,BKCOUNT                                                       
         ZIC   R2,CNTCHUNK         AND IF REACHED THE NUMBER OF RECORDS         
         BCT   R2,PUTMQ30          TO SEND PER MQ CHUNK                         
         BAS   RE,CLOSEMQ          CLOSE MQ                                     
PUTMQ30  STC   R2,CNTCHUNK         DECREMENT # OF RECS IN CURRENT CHUNK         
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO BUILD ENTRY LENGTH IN R3                              
*              ON ENTRY ... R2=A(FILDATA)                                       
*              ON EXIT  ... R3=L'ENTRY                                          
         SPACE 1                                                                
BLDLEN   NTR1                                                                   
         XR    R3,R3               BUILD ENTRY LENGTH IN RF                     
         SPACE 1                                                                
         CLI   FILLEN,C' '                                                      
         BE    BLDLEN10                                                         
         ZIC   R3,FILLEN                                                        
         SHI   R3,X'F0'            MULTIPLY FIRST CHARACTER                     
         MHI   R3,100              OF LENGTH BY 100                             
         SPACE 1                                                                
BLDLEN10 CLI   FILLEN+1,C' '                                                    
         BE    BLDLEN20                                                         
         ZIC   RE,FILLEN+1                                                      
         SHI   RE,X'F0'            MULTIPLY SECOND CHARACTER                    
         MHI   RE,10               OF LENGTH BY 10                              
         AR    R3,RE               AND ADD TO ENTRY LENGTH                      
         SPACE 1                                                                
BLDLEN20 ZIC   RE,FILLEN+2         MULTIPLY THIRD CHARACTER                     
         SHI   RE,X'F0'            OF LENGTH BY 1                               
         AR    R3,RE               AND ADD TO ENTRY LENGTH                      
         B     XIT                                                              
         EJECT                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO CLEAR BACKUP DATA ENTRY                               
         SPACE 1                                                                
CLRBK    NTR1                                                                   
         LA    R2,BAKDATA                                                       
         LHI   RE,4                                                             
         SPACE 1                                                                
CLRBK10  MVC   0(BAKLNQ/4,R2),SPACES                                            
         LA    R2,BAKLNQ/2(R2)                                                  
         BCT   RE,CLRBK10                                                       
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PUT BACKUP DATA ENTRY TO BACKUP FILE                  
         SPACE 1                                                                
PUTBK    NTR1                                                                   
         PUT   BAKFILE,BAKDATA                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLOSE MQ                                              
         SPACE 1                                                                
CLOSEMQ  NTR1                                                                   
         TM    MQSTATUS,MQOPEN     EXIT IF MQ HAS NOT BEEN OPENED               
         BZ    XIT                                                              
         SPACE 1                                                                
         USING FILDATAD,R2                                                      
         LA    R2,CLOSEMSG         PUT CLOSING MESSAGE TO MQ                    
         LHI   RF,CLMSGLNQ                                                      
         EDIT  (RF),FILLEN                                                      
         BRAS  RE,PUTMQ                                                         
         DROP  R2                                                               
         SPACE 1                                                                
         GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         SPACE 1                                                                
         MVC   AUTOREAS,CLOSFAIL  IF MQ CLOSE FAILS                             
         CLI   DMCB+8,0           SEND EMAIL NOTIFICATION AND DUMP              
         BNE   ERREND                                                           
         SPACE 1                                                                
         NI    MQSTATUS,X'FF'-MQOPEN                                            
         SPACE 1                                                                
         BAS   RE,CLRBK                                                         
         MVC   BAKDATA(PPCLNQ),PPC PUT "ALL PREVIOUS PUTS COMMITTED"            
         BAS   RE,PUTBK            MESSAGE TO BACKUP FILE                       
         SPACE 1                                                                
         BAS   RE,CLRBK                                                         
         MVC   BAKDATA(MSCLNQ),MSC PUT "MQ SUCCESSFULLY CLOSED"                 
         BAS   RE,PUTBK            MESSAGE TO BACKUP FILE                       
         SPACE 1                                                                
         BAS   RE,CLOSINP          CLOSE THE FILE FOR INPUT                     
         BAS   RE,OPENUPD          AND REOPEN IT FOR UPDATE                     
         SPACE 1                                                                
         USING FILDATAD,R2                                                      
         L     R3,MQCOUNT          R3=# OF FILE ENTRIES TO MARK OFF             
CMQ10    GET   UPDFILE             GET XML FILE ENTRY                           
         LR    R2,R1                                                            
         CLI   FILSTAT,FILSTMQ     IF ENTRY SHOULD NOT BE SENT TO MQ            
         BE    CMQ10               DO NOT MARK IT AS DO NO SEND AGAIN           
         MVI   FILSTAT,FILSTMQ     ELSE, MARK IT SENT TO MQ                     
         PUTX  UPDFILE             AND PUT IT BACK TO FILE                      
         BCT   R3,CMQ10                                                         
         DROP  R2                                                               
         SPACE 1                                                                
         BAS   RE,CLOSUPD          CLOSE THE FILE FOR UPDATE                    
         BAS   RE,OPENINP          AND REOPEN FILE FOR INPUT                    
         SPACE 1                                                                
         XC    MQCOUNT,MQCOUNT                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLOSE BACKUP FILE                                     
         SPACE 1                                                                
CLOSEBK  NTR1                                                                   
         BAS   RE,CLRBK          SEND RECORD COUNT TO BACKUP FILE               
         MVI   BAKSTAT,C'X'                                                     
         EDIT  BKCOUNT,(10,BAKENTRY),ALIGN=LEFT                                 
         BAS   RE,PUTBK                                                         
         SPACE 1                                                                
         CLOSE BAKFILE           AND CLOSE IT                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         NI    BKSTATUS,X'FF'-BKOPEN                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLOSE FILE FOR COUNTING                               
         SPACE 1                                                                
CLOSCNT  NTR1                                                                   
         CLOSE CNTFILE             CLOSE FILE FOR COUNTING                      
         NI    FILESTAT,X'FF'-FILECOP                                           
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLOSE FILE FOR INPUT                                  
         SPACE 1                                                                
CLOSINP  NTR1                                                                   
         CLOSE INPFILE             CLOSE FILE FOR INPUT                         
         NI    FILESTAT,X'FF'-FILEIOP                                           
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLOSE FILE FOR UPDATE                                 
         SPACE 1                                                                
CLOSUPD  NTR1                                                                   
         CLOSE UPDFILE             CLOSE FILE FOR UPDATE                        
         NI    FILESTAT,X'FF'-FILEUOP                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SEND OUT FAILURE EMAIL AND DUMP                       
         SPACE 1                                                                
ERREND   GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(AUTOLENG),AUTONOTE)                 
         DC    H'00'                                                            
         EJECT                                                                  
*              CONDITION CODE & EXIT ROUTINE                                    
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1  REGS=(R3,R4)                                                     
         DROP  R4                                                               
         EJECT                                                                  
*              TABLES, CONSTANTS, ETC.                                          
         SPACE 1                                                                
SOFTVARS DC    AL1(H2W),C'TALHOLD*********',C'holding fee',C'HF'                
         DC    X'FF'                                                            
         SPACE 4                                                                
CNTFILE  DCB   DDNAME=WEBFIL,DSORG=PS,MACRF=(GM),EODAD=CMSGS30,        X        
               RECFM=FB,LRECL=500                                               
         SPACE 1                                                                
INPFILE  DCB   DDNAME=WEBFIL,DSORG=PS,MACRF=(GM),EODAD=F2M20,          X        
               RECFM=FB,LRECL=500                                               
         SPACE 1                                                                
UPDFILE  DCB   DDNAME=WEBFIL,DSORG=PS,RECFM=FB,LRECL=500,BLKSIZE=0,    X        
               MACRF=(GL,PL),EODAD=MPREV40                                      
         SPACE 2                                                                
BAKFILE  DCB   DDNAME=BAKFIL,DSORG=PS,RECFM=FB,LRECL=500,BLKSIZE=0,    X        
               MACRF=PM                                                         
         SPACE 4                                                                
MSC      DC    C'X'                                                             
         DC    3X'00'                                                           
MQSCLOS  DC    C'MQ SUCCESSFULLY CLOSED'                                        
MSCLNQ   EQU   *-MSC                                                            
         SPACE 2                                                                
PPC      DC    C'X'                                                             
         DC    3X'00'                                                           
PPUTCOM  DC    C'ALL PREVIOUS PUTS COMMITTED'                                   
PPCLNQ   EQU   *-PPC                                                            
         SPACE 2                                                                
GROUPEND DC    C'ÙÙ></ddoc>'                                                    
         SPACE 2                                                                
WEBHEAD  DC    C'TAL.TA0W '                                                     
WEBBK    DC    C'B.D'                                                           
WEBTIME  DC    C'.T'                                                            
         SPACE 2                                                                
OPENMSG1 DS    X                 ENTRY STATUS                                   
         DS    XL3               ENTRY LENGTH                                   
OMSG1    DC    C'<dmessage>'                                                    
OMSG1LNQ EQU   *-OMSG1                                                          
         SPACE 1                                                                
OPENMSG2 DS    X                 ENTRY STATUS                                   
         DS    XL3               ENTRY LENGTH                                   
OMSG2    DC    C'<dhead><doctypeId>'                                            
OM2SOFT  DC    CL11'           '                                                
         DC    C'</doctypeId><sourceId>'                                        
         DC    C'mainframe</sourceId><formatId>fixed-length text'               
         DC    C'</formatId></dhead>'                                           
OMSG2LNQ EQU   *-OMSG2                                                          
         SPACE 1                                                                
CLOSEMSG DS    X                 ENTRY STATUS                                   
         DS    XL3               ENTRY LENGTH                                   
CMSG     DC    C'</dmessage>'                                                   
CLMSGLNQ EQU   *-CMSG                                                           
         SPACE 2                                                                
AUTONOTE DC    C'AUTONOTE*GHOA,MZEI,JBAS'                                       
AUTOREAS DS    CL15                                                             
AUTOLENG EQU   *-AUTONOTE                                                       
OPENFAIL DC    CL(L'AUTOREAS)'MQ OPEN FAILED'                                   
PUTFAIL  DC    CL(L'AUTOREAS)'MQ PUT ERROR'                                     
CLOSFAIL DC    CL(L'AUTOREAS)'MQ CLOSE FAILED'                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
RECD     DSECT                                                                  
PERCHUNK DS    X                 NUMBER OF RECORDS PER MQ CHUNK                 
CNTCHUNK DS    X                 RECORD COUNT FOR CURRENT MQ CHUNK              
         SPACE 1                                                                
MQSTATUS DS    X                 CURRENT MQ STATUS                              
MQOPEN   EQU   X'80'             MQ HAS BEEN OPENED                             
         SPACE 1                                                                
BKSTATUS DS    X                 CURRENT BACKUP FILE STATUS                     
BKOPEN   EQU   X'80'             BACKUP FILE HAS BEEN OPENED                    
         SPACE 1                                                                
FILESTAT DS    X                 CURRENT FILE STATUS                            
FILECOP  EQU   X'80'             FILE OPEN FOR COUNTING                         
FILEIOP  EQU   X'40'             FILE OPEN FOR INPUT                            
FILEUOP  EQU   X'20'             FILE OPEN FOR UPDATE                           
FILEACM  EQU   X'10'             FILE OPEN TO MARK POST-COMMIT ENTRIES          
         SPACE 1                                                                
DYNALLOC DS    A                 DYNAMIC ALLOCATION OF DATASET                  
         SPACE 1                                                                
MQCOUNT  DS    F                 ENTRIES SENT TO MQ SINCE IT WAS OPENED         
BKCOUNT  DS    F                 ENTRIES SENT TO BACKUP FILE                    
         SPACE 1                                                                
CLCOUNT  DS    F                 "MQ SUCCESSFULLY CLOSED" COUNT                 
CMCOUNT  DS    F                 "ALL PREVIOUS PUTS COMMITTED" COUNT            
RECDLNQ  EQU   *-RECD                                                           
         SPACE 1                                                                
FILDATA  DS    XL500             ENTRY READ FROM FILE                           
         SPACE 1                                                                
BAKDATA  DS    0X                BACKUP FILE ENTRY                              
BAKSTAT  DS    X                 ENTRY STATUS                                   
BAKLEN   DS    XL3               ENTRY LENGTH                                   
BAKENTRY DS    XL400             ENTRY RECORD AREA                              
BAKLNQ   EQU   *-BAKDATA         BACKUP FILE ENTRY LENGTH                       
         EJECT                                                                  
*              DSECT TO COVER SOFTVARS TABLE ENTRIES                            
         SPACE 1                                                                
SOFTVARD DSECT                                                                  
SVRECNUM DS    AL1               RECORD NUMBER EQUATE                           
SVMQOPLB DS    CL16              MQ OPEN LABEL                                  
SVPTOPLB DS    CL11              MQ PUT OPEN LABEL                              
SVBKFLLB DS    CL2               BACKUP FILE LABEL                              
SVLNQ    EQU   *-SOFTVARD                                                       
         SPACE 2                                                                
*              DSECT TO COVER ENTRY READ FROM XML FILE                          
         SPACE 1                                                                
FILDATAD DSECT                                                                  
FILSTAT  DS    X                 ENTRY STATUS                                   
FILSTMQ  EQU   C'X'              ENTRY SHOULD NOT BE SENT TO MQ                 
FILLEN   DS    XL3               ENTRY LENGTH                                   
FILENTRY DS    0X                ENTRY DATA                                     
         SPACE 2                                                                
*              DSECT TO COVER BACKUP FILE NAME                                  
         SPACE 1                                                                
WEBNAMD  DSECT                                                                  
WNHEAD   DS    CL8                TAL.TA0W                                      
WNRUNSYS DS    CL1                RUN ON SYSTEM                                 
WNRUNTYP DS    CL2                RUN TYPE                                      
WNRUNWHN DS    CL1                RUN WHEN                                      
WNDOT2   DS    CL1                DOT 2                                         
WNDATEHD DS    CL1                DATE HEADER                                   
WNRUNDTE DS    CL6                RUN DATE                                      
WNDOT3   DS    CL1                DOT 3                                         
WNRUNTHD DS    CL1                DATE HEADER                                   
WNRUNTIM DS    CL6                RUN TIME                                      
WNLNQ    EQU   *-WEBNAMD                                                        
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP5A   01/19/06'                                      
         END                                                                    
