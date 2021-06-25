*          DATA SET TAREP57    AT LEVEL 002 AS OF 01/07/04                      
*PHASE T70356A                                                                  
         TITLE 'T70356 - REPORT OF DELETED INVOICE RECORDS'                     
T70356   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70356                                                         
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
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         IF CALLED WITH VALIDATE KEY                  
         BNE   *+12                                                             
         BAS   RE,VK               VALIDATE THE KEY                             
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       IF CALLED WITH PRINTREP                      
         BNE   *+8                                                              
         BAS   RE,PREP             PROCESS THE REPORT                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE 1                                                                
VK       NTR1                                                                   
         BAS   RE,VALOPT           MERELY VALIDATE THE OPTIONS                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE OPTIONS                                      
VALOPT   NTR1                                                                   
         LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0 = NUM OF SCAN BLOCK ENTRIES               
*                                                                               
VOPT4    CLC   =C'TRACE',SCDATA1                                                
         BNE   VOPT5                                                            
         CLI   SCDATA2,C'Y'        IF TRACING REQUESTED                         
         BNE   VOPT8                                                            
         OI    OPTION,OPTTRACE     SAVE INDICATOR                               
*                                                                               
VOPT5    CLC   =C'RECOVER',SCDATA1                                              
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'        IF RECOVERY FILE READ REQUESTED              
         BNE   VOPT8                                                            
         OI    OPTION,OPTRECOV     SAVE INDICATOR                               
*                                                                               
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT ELEMENT                         
         BCT   R0,VOPT4                                                         
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                  DROP THE SCAND DSECT                         
         EJECT                                                                  
         SPACE 1                                                                
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OPEN  (RECVIN,(INPUT))    OPEN THE INPUT FILE                          
         BAS   RE,PUTSORT          RTN TO READ REC & PUT TO SORTER              
         BAS   RE,GETSORT          RTN TO GET RECS FROM SORTER & PRINT          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT DATA TO SORTER                                    
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
PUT10    XC    SORTREC,SORTREC                                                  
         LA    R5,SORTREC          R5 = A(RECORD TO BE SENT TO SORTER)          
         USING SORTD,R5                                                         
*                                                                               
         USING TLIND,R4                                                         
         TM    OPTION,OPTRECOV     IF NOT READING RECOVERY FILE                 
         BO    PUT30                                                            
         LA    R4,KEY              READ ALL INVOICE RECORD KEYS                 
         XC    KEY,KEY                                                          
         MVI   TLINCD,TLINCDQ                                                   
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         B     PUT21                                                            
PUT20    OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
PUT21    LA    R4,KEY                                                           
         CLI   KEY,TLINCDQ                                                      
         BNE   XIT                                                              
         USING TLDRD,R4                                                         
         TM    TLDRSTAT,X'01'      ONLY INTERESTED IN SOFT DELETED              
         BO    PUT22                                                            
         TM    TLDRSTAT,X'80'      AND ACTUALLY DELETED ONES                    
         BZ    PUT20                                                            
PUT22    GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         B     PUT40                                                            
         DROP  R4                                                               
*                                                                               
PUT30    LA    R2,RCVREC           IF READING RECOVERY FILE                     
         GET   RECVIN,(R2)         R2 = A(INPUT FILE)                           
         LA    R2,4(R2)            R2 = A(RECOVERY HEADER)                      
         USING RCVD,R2                                                          
         TM    OPTION,OPTRECOV                                                  
         BZ    PUT30                                                            
         CLI   RFILTY,X'72'        ONLY INTERESTED IN TALFILE                   
         BNE   PUT10                                                            
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         USING TLIND,R4                                                         
         CLI   TLINCD,TLINCDQ      ONLY INTERESTED IN INVOICE RECORDS           
         BNE   PUT10                                                            
         CLI   RRECTY,X'02'        THAT WERE CHANGED                            
         BNE   PUT10                                                            
         TM    TLINSTAT,X'01'                                                   
         BO    PUT40                                                            
         TM    TLINSTAT,X'80'      AND ARE MARKED FOR DELETE                    
         BNO   PUT10                                                            
*                                                                               
PUT40    MVC   SORTAGY,TLINAGY     SAVE AGENCY                                  
         MVC   TEMPINV,TLININV     AND INVOICE NUMBER INTO SORT KEY             
         XC    TEMPINV,=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,TEMPINV,SORTINV,DATCON                              
         DROP  R4                                                               
*                                                                               
         USING TAACD,R4                                                         
         MVI   ELCODE,TAACELQ      ONLY INTERESTED IF INVOICE IS                
         BAS   RE,GETEL            MARKED AS DELETED                            
         B     *+8                                                              
PUT60    BAS   RE,NEXTEL                                                        
         BE    PUT65                                                            
         TM    OPTION,OPTRECOV                                                  
         BO    PUT10                                                            
         B     PUT20                                                            
PUT65    CLI   TAACSCR,X'1F'                                                    
         BNE   PUT60                                                            
         TM    OPTION,OPTRECOV                                                  
         BO    PUT70                                                            
         CLC   TAACCDTE,TGTODAY1                                                
         BNE   PUT60                                                            
PUT70    MVC   SORTSTAF,TAACSTAF                                                
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,SORTDTE)                             
         GOTO1 TIMECON,DMCB,TAACCTIM,TAACCDTE,(8,SORTTIM)                       
         DROP  R4                                                               
         SPACE                                                                  
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         TM    OPTION,OPTRECOV                                                  
         BO    PUT10                                                            
         B     PUT20                                                            
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO GET RECORDS FROM SORTER & PRINT THEM                  
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         USING PRNTD,R3                                                         
*                                                                               
GET60    LA    R3,P                R3 = A(ADDRESS OF PRINT LINE)                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R5,15,4(R1)         R5 = A(SORTED RECORD)                        
         BZ    GETXIT                                                           
*                                                                               
         MVC   PRNTAGY,SORTAGY                                                  
         MVC   PRNTINV,SORTINV                                                  
         MVC   PRNTSTAF,SORTSTAF                                                
         MVC   PRNTDTE,SORTDTE                                                  
         MVC   PRNTTIM,SORTTIM                                                  
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT          FOR BLANK LINE                               
         B     GET60                                                            
*                                                                               
GETXIT   GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT LINE OF OUTPUT                                  
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ERROR ROUTINES                                                   
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 2                                                                
*              CONDITION CODE & EXIT ROUTINE                                    
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              STANDARD ROUTINES                                                
         SPACE 1                                                                
         GETEL  R4,DATADISP,ELCODE                                              
         SPACE 2                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,                                 X        
               MACRF=GM,EODAD=XIT                                               
SORTCARD DC    CL80'SORT FIELDS=(1,12,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
         SPACE 1                                                                
OPTTRACE EQU   X'80'                                                            
OPTRECOV EQU   X'40'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECIFICATIONS                                            
         SPACE 1                                                                
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H1,46,C'REPORT OF DELETED INVOICE RECORDS'                       
*                                                                               
         SSPEC H7,02,C'AGENCY'                                                  
         SSPEC H8,02,C'------'                                                  
         SSPEC H7,10,C'INVOICE'                                                 
         SSPEC H8,10,C'-------'                                                 
         SSPEC H7,19,C'STAFF'                                                   
         SSPEC H8,19,C'--------'                                                
         SSPEC H7,29,C'DATE'                                                    
         SSPEC H8,29,C'--------'                                                
         SSPEC H7,39,C'TIME'                                                    
         SSPEC H8,39,C'--------'                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
RECD     DSECT                                                                  
TEMPINV  DS    CL6                                                              
COPYCNT  DS    X                                                                
TACOUNT  DS    X                                                                
SORTREC  DS    XL200                                                            
RCVREC   DS    0H                                                               
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
RHEAD    DS    XL24                                                             
RDATA    DS    2500C                                                            
         SPACE 2                                                                
*              DSECT TO COVER SORT KEY                                          
         SPACE 1                                                                
SORTD    DSECT                                                                  
SORTAGY  DS    CL6                 AGENCY                                       
SORTINV  DS    CL6                 COMMERCIAL ID                                
SORKEYQ  EQU   *-SORTD                                                          
SORTSTAF DS    CL8                 STAFF ID                                     
SORTDTE  DS    XL8                 DATE                                         
SORTTIM  DS    XL8                 TIME                                         
SORTDQ   EQU   *-SORTD                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
         DS    CL1                                                              
PRNTAGY  DS    CL6        AGENCY                                                
         DS    CL2                                                              
PRNTINV  DS    CL6        INVOICE                                               
         DS    CL3                                                              
PRNTSTAF DS    CL8        STAFF                                                 
         DS    CL2                                                              
PRNTDTE  DS    CL8        DATE                                                  
         DS    CL2                                                              
PRNTTIM  DS    CL8        TIME                                                  
         ORG                                                                    
         EJECT                                                                  
RCVD     DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP57   01/07/04'                                      
         END                                                                    
