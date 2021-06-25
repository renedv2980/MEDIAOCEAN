*          DATA SET TAREP70    AT LEVEL 003 AS OF 06/17/14                      
*PHASE T70370E,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70370 - PMUSIC COMMERCIAL RECORD ACTIVITY REPORT'              
T70370   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70302,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,PRINTREP                                                    
         JNE   YES                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
         J     XIT                                                              
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         LTORG                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
PREP     NTR1                                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN                                                      
         BAS   RE,DWNHDR           DOWNLOAD COLUMN HEADERS                      
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OPEN  (RECVIN,(INPUT))    OPEN THE INPUT FILE                          
*                                                                               
         BAS   RE,PROCRCV          PROCESS RECOVERY FILE                        
         BAS   RE,GETSORT          GET RECS FROM SORTER & DOWNLOAD              
*                                                                               
PREPX    BAS   RE,EOLDOWN                                                       
         J     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
*        DOWNLOAD COLUMN HEADERS                                      *         
***********************************************************************         
DWNHDR   NTR1                                                                   
         MVC   WORK(10),=C'Agency       '                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         MVC   WORK(10),=C'Commercial ID'                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         MVC   WORK(10),=C'Client       '                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         MVC   WORK(10),=C'Version      '                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         MVC   WORK(10),=C'Action       '                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         MVC   WORK(10),=C'Music Code   '                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         MVC   WORK(10),=C'Staff ID     '                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         MVC   WORK(10),=C'Date         '                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         MVC   WORK(10),=C'Time         '                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),13                                     
         BAS   RE,EOLDOWN                                                       
DWNHDRX  J     XIT                                                              
***********************************************************************         
*        READ RECOVERY FILE AND PUT RECORDS TO SORTER                 *         
***********************************************************************         
PROCRCV  NTR1                                                                   
PRCVNXT  LA    R2,RCVREC           R2 = A(INPUT AREA)                           
         GET   RECVIN,(R2)                                                      
         LA    R2,4(R2)            R2 = A(RECOVERY HEADER)                      
         USING RCVD,R2                                                          
         XC    SORTREC,SORTREC                                                  
S        USING SRTD,SORTREC                                                     
*                                                                               
         CLI   RFILTY,X'72'        ONLY INTERESTED IN TALFILE                   
         JNE   PRCVNXT                                                          
*                                                                               
         MVC   SVTYPE,RRECTY                                                    
*                                                                               
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         USING TLCOD,R4                                                         
         CLI   TLCOCD,TLCOCDQ      ONLY INTERESTED IN COMMERCIALS               
         JNE   PRCVNXT                                                          
*                                                                               
         CLI   RRECTY,X'01'        IF THIS IS A COPY                            
         JE    PRCVCPY                                                          
         CLI   RRECTY,X'02'        IF THIS IS A CHANGE?                         
         JE    PRCVCHG                                                          
         J     PRCVNXT                                                          
*                                                                               
PRCVCPY  XC    SVMUS(SVMUSLQ),SVMUS                                             
         BAS   RE,GETVALS          GET VALUES FROM COPY RECORD                  
         J     PRCVNXT                                                          
*                                                                               
PRCVCHG  BAS   RE,GETVALS          GET VALUES FROM CHANGE RECORD                
         BAS   RE,COMPARE          COMPARE CHANGE RECORD WITH COPY              
         J     PRCVNXT                                                          
***********************************************************************         
*        GET RECORDS FROM SORTER AND DOWNLOAD                         *         
***********************************************************************         
GETSORT  NTR1                                                                   
GSORT05  GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R2,15,4(R1)         R2 = A(SORTED RECORD)                        
         JZ    GETSORTX                                                         
         USING SRTD,R2                                                          
*                                                                               
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTAGY),L'SRTAGY                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTCMML),L'SRTCMML                           
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTCLI),L'SRTCLI                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTVER),L'SRTVER                             
*        GOTO1 OUTPDOWN,DMCB,(C'T',SRTFLD),L'SRTFLD                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTACT),L'SRTACT                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTMUS),L'SRTMUS                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTSTAFF),L'SRTSTAFF                         
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTDATE),L'SRTDATE                           
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTTIME),L'SRTTIME                           
         BAS   RE,EOLDOWN                                                       
         J     GSORT05                                                          
         DROP  R2                                                               
*                                                                               
GETSORTX GOTO1 SORTER,DMCB,=C'END'                                              
         J     XIT                                                              
***********************************************************************         
*        GET VALUES FROM RECOVERY RECORD                              *         
*        AIO = AIO1 FOR COPY RECORD                                   *         
*        AIO = AIO2 FOR CHANGE RECORD                                 *         
***********************************************************************         
GETVALS  NTR1                                                                   
         LA    R4,RDATA                                                         
         USING TLCOD,R4                                                         
*                                                                               
         MVC   SVAGY,TLCOAGY       AGENCY                                       
         MVC   SVCLI,TLCOCLI       CLIENT                                       
         EDIT  TLCOVER,SVVER,ALIGN=LEFT   VERSION                               
*                                                                               
         USING TACOD,R4                                                         
         LA    R4,RDATA                                                         
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCMML,TACOCID      COMMERCIAL ID                                
*                                                                               
         LA    R5,SVOMUS                                                        
         CLI   SVTYPE,X'02'        IF THIS IS A CHANGE?                         
         JNE   *+8                                                              
         LA    R5,SVNMUS                                                        
*                                                                               
         LA    R6,4                                                             
         USING TACPD,R4                                                         
         LA    R4,RDATA                                                         
         MVI   ELCODE,TACPELQ                                                   
         BAS   RE,GETEL                                                         
         J     *+8                                                              
GETV10   BAS   RE,NEXTEL                                                        
         JNE   GETVALSX                                                         
         MVC   0(L'TACPMUS,R5),TACPMUS                                          
         AHI   R5,L'TACPMUS                                                     
         BCT   R6,GETV10                                                        
         DROP  R4                                                               
*                                                                               
GETVALSX J     XIT                                                              
***********************************************************************         
*        COMPARE CHANGE RECORD TO COPY                                *         
*        AIO1 = COPY RECORD VALUES                                    *         
*        AIO2 = CHANGE RECORD VALUES                                  *         
***********************************************************************         
COMPARE  NTR1                                                                   
         LA    R2,SVOMUS           COPY MUSIC CODES                             
         LA    R3,4                                                             
COMP10   LA    R5,SVNMUS           CHANGE MUSIC CODES                           
         LA    R6,4                                                             
COMP20   CLC   0(8,R2),0(R5)                                                    
         JNE   COMP30                                                           
         XC    0(8,R2),0(R2)                                                    
         XC    0(8,R5),0(R5)                                                    
         J     COMP40                                                           
*                                                                               
COMP30   AHI   R5,8                                                             
         BCT   R6,COMP20                                                        
COMP40   AHI   R2,8                IT WAS REMOVED                               
         BCT   R3,COMP10                                                        
*                                                                               
* AT THIS POINT, SVOMUS HAS THE REMOVED MUSIC CODES                             
* AND SVNMUS HAS THE ADDED MUSIC CODES                                          
*                                                                               
         MVI   SVFLDNUM,1                                                       
         LA    R2,SVOMUS                                                        
         LA    R3,4                                                             
*                                                                               
COMP50   OC    0(8,R2),0(R2)                                                    
         JZ    COMP60                                                           
*                                                                               
         XC    SORTREC,SORTREC                                                  
S        USING SRTD,SORTREC                                                     
*                                                                               
         MVC   S.SRTACT,=C'REMOVED '                                            
         MVC   S.SRTMUS,0(R2)      MUSIC CODE                                   
         BAS   RE,PUTSORT                                                       
*                                                                               
COMP60   AHI   R2,8                                                             
         ZIC   RF,SVFLDNUM                                                      
         AHI   RF,1                                                             
         STC   RF,SVFLDNUM                                                      
         BCT   R3,COMP50                                                        
*                                                                               
         MVI   SVFLDNUM,1                                                       
         LA    R2,SVNMUS                                                        
         LA    R3,4                                                             
*                                                                               
COMP70   OC    0(8,R2),0(R2)                                                    
         JZ    COMP80                                                           
*                                                                               
         XC    SORTREC,SORTREC                                                  
S        USING SRTD,SORTREC                                                     
*                                                                               
         MVC   S.SRTACT,=C'ADDED   '                                            
         MVC   S.SRTMUS,0(R2)      MUSIC CODE                                   
         BAS   RE,PUTSORT                                                       
*                                                                               
         ZIC   RF,SVFLDNUM                                                      
         AHI   RF,1                                                             
         STC   RF,SVFLDNUM                                                      
*                                                                               
COMP80   AHI   R2,8                                                             
         BCT   R3,COMP70                                                        
*                                                                               
COMPAREX J     XIT                                                              
***********************************************************************         
*        PUT VALUES TO SORTER                                         *         
***********************************************************************         
PUTSORT  NTR1                                                                   
S        USING SRTD,SORTREC                                                     
*                                                                               
         MVC   S.SRTAGY,SVAGY      AGENCY                                       
         MVC   S.SRTCMML,SVCMML    COMMERCIAL ID                                
         MVC   S.SRTCLI,SVCLI      CLIENT                                       
         MVC   S.SRTVER,SVVER      VERSION                                      
         MVC   S.SRTFLD,=C'MUS '   FIELD                                        
*                                                                               
         EDIT  SVFLDNUM,FULL,ALIGN=LEFT   FIELD #                               
         MVC   S.SRTFLD+3(1),FULL                                               
*                                                                               
         LA    R4,RDATA                                                         
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         JNE   PUTS10                                                           
         USING TAACD,R4                                                         
*                                                                               
         MVC   S.SRTSTAFF,TAACSTAF   STAFF ID                                   
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,S.SRTDATE) DATE                      
         GOTO1 TIMECON,DMCB,TAACCTIM,TAACCDTE,(8,S.SRTTIME) TIME                
*                                                                               
PUTS10   GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         J     XIT                                                              
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
         DROP  R5                                                               
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
*                     R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
         USING DLCBD,R5                                                         
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
*                                                                               
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
*                                                                               
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
         DROP  R5                                                               
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
         USING DLCBD,R5                                                         
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
*        END DOWNLOAD                                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
         USING DLCBD,R5                                                         
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
         LTORG                                                                  
***********************************************************************         
*        SORTER CARDS                                                 *         
***********************************************************************         
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,MACRF=GM,EODAD=XIT                        
SORTCARD DC    CL80'SORT FIELDS=(1,28,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
MYD      DSECT                                                                  
SORTREC  DS    XL200                                                            
*                                                                               
CURVALS  DS    0X                                                               
CURAGY   DS    CL6                 AGENCY                                       
CURCMML  DS    CL12                COMMERCIAL ID                                
CURCLI   DS    CL6                 CLIENT                                       
CURVER   DS    CL3                 VERSION                                      
CURDATE  DS    CL8                 DATE                                         
CURTIME  DS    CL8                 TIME                                         
CURFLD   DS    CL4                 FIELD                                        
CURACT   DS    CL8                 ACTION                                       
CUROVAL  DS    CL72                OLD VALUE                                    
CURNVAL  DS    CL72                NEW VALUE                                    
CURSTAFF DS    CL8                 STAFF ID                                     
CURVALNQ EQU   *-CURVALS                                                        
*                                                                               
SVVALS   DS    0C                                                               
SVAGY    DS    CL6                 AGENCY                                       
SVCMML   DS    CL12                COMMERCIAL ID                                
SVCLI    DS    CL6                 CLIENT                                       
SVVER    DS    CL3                 VERSION                                      
SVVALSLQ EQU   *-SVVALS                                                         
*                                                                               
*                                                                               
SVMUS    DS    0C                                                               
SVOMUS   DS    0C                                                               
SVOMUS1  DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVOMUS2  DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVOMUS3  DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVOMUS4  DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVOMUSLQ EQU   *-SVOMUS                                                         
*                                                                               
SVNMUS   DS    0C                                                               
SVNMUS1  DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVNMUS2  DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVNMUS3  DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVNMUS4  DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVNMUSLQ EQU   *-SVNMUS                                                         
SVMUSLQ  EQU   *-SVMUS                                                          
*                                                                               
SVTYPE   DS    XL1                                                              
SVFLDNUM DS    XL1                                                              
*                                                                               
RCVREC   DS    0H                  RECOVERY RECORD                              
RLEN     DS    H                                                                
         DS    H                                                                
RHEAD    DS    XL24                                                             
RDATA    DS    2500C                                                            
*                                                                               
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
*                                                                               
SRTD     DSECT                                                                  
SRTAGY   DS    CL6                 AGENCY                                       
SRTCMML  DS    CL12                COMMERCIAL ID                                
SRTCLI   DS    CL6                 CLIENT                                       
SRTVER   DS    CL3                 VERSION                                      
SRTDATE  DS    CL8                 DATE                                         
SRTTIME  DS    CL8                 TIME                                         
SRTFLD   DS    CL4                 FIELD                                        
SRTKEYQ  EQU   *-SRTD                                                           
SRTACT   DS    CL8                 ACTION                                       
SRTMUS   DS    CL8                 MUSIC CODE                                   
SRTSTAFF DS    CL8                 STAFF ID                                     
SRTDLQ   EQU   *-SRTD                                                           
*                                                                               
* MUSIC FIELDS FROM RECORD                                                      
*                                                                               
* IF A FIELD IS ADDED IN SVVALD, MAKE SURE BUMP IS TAKEN INTO                   
* ACCOUNT IN "COMPARE" ROUTINE                                                  
*                                                                               
*                                                                               
RCVD     DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
RCVEXTD  DSECT                                                                  
       ++INCLUDE DMRCVREXT                                                      
*                                                                               
       ++INCLUDE TAREPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEAD                                                       
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAREP70   06/17/14'                                      
         END                                                                    
