*          DATA SET TAREP02    AT LEVEL 065 AS OF 10/08/04                      
*PHASE T70302A,*                                                                
         TITLE 'T70302 - USAGE HISTORY FILE FIX'                                
T70302A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70302                                                         
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
         USING MYD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VALOPT                                                        
         BAS   RE,PREP             READ DISK,UPDATE W4'S,PRINT REPORT           
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
VALOPT   NTR1                                                                   
         MVI   TR,0                                                             
         SPACE 1                                                                
         LA    R2,SPLOPTH          R2=A(OPTIONS FIELD)                          
         CLI   5(R2),0                                                          
         BE    VALOPTX                                                          
         SPACE 1                                                                
         LA    R3,BLOCK                                                         
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
         SPACE 1                                                                
VALOPT10 CLC   =C'TRACE',SCDATA1   TRACE OPTION                                 
         BNE   INVERR                                                           
         CLI   SCDATA2,C'N'                                                     
         BE    VALOPT20                                                         
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         MVI   TR,C'Y'                                                          
         SPACE 1                                                                
VALOPT20 LA    R3,SCANNEXT                                                      
         BCT   R0,VALOPT10                                                      
VALOPTX  B     XIT                                                              
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
         DROP  R3                                                               
         EJECT                                                                  
*              READ FROM DISK,UPDATE CSYS'S AND PRINT REPORT                    
         SPACE                                                                  
PREP     NTR1                                                                   
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         SPACE 1                                                                
         USING PRINTD,R2           R2=A(PRINT LINE)                             
         LA    R2,P                                                             
         SPACE 1                                                                
         XC    TCOUNT,TCOUNT                                                    
         SPACE 1                                                                
         USING TLUHD,R3                                                         
         LA    R3,KEY            READ ALL USAGE HISTORY RECORDS                 
         XC    KEY,KEY                                                          
         MVI   TLUHCD,TLUHCDQ                                                   
         GOTO1 HIGH                                                             
         B     PREP20                                                           
PREP10   GOTO1 SEQ                                                              
PREP20   LA    R3,KEY                                                           
         CLI   KEY,TLUHCDQ                                                      
         BNE   PREP90                                                           
         CLC   TLUHUSE,=C'CBL'   FOR USE TYPE CABLE                             
         BE    PREP30                                                           
         CLC   TLUHUSE,=C'SCB'   SPANISH CABLE                                  
         BE    PREP30                                                           
         CLC   TLUHUSE,=C'WSP'   WILDSPOT                                       
         BE    PREP30                                                           
         CLC   TLUHUSE,=C'SWS'   SPANISH WILDSPOT                               
         BE    PREP30                                                           
         CLC   TLUHUSE,=C'WSC'   CANADIAN WILDSPOT                              
         BE    PREP30                                                           
         CLC   TLUHUSE,=C'ADW'   ADDENDUM WILDSPOT                              
         BE    PREP30                                                           
         CLC   TLUHUSE,=C'LCB'   AND LOCAL CABLE                                
         BNE   PREP10                                                           
         SPACE 1                                                                
PREP30   GOTO1 GETREC            GET USAGE HISTORY RECORD                       
         SPACE 1                                                                
         L     R4,AIO            ONLY PROCESS THE RECORD                        
         MVI   ELCODE,TAMTELQ    IF IT HAS A CNET/CSYS/MARKET                   
         BAS   RE,GETEL          ELEMENT                                        
         BNE   PREP10                                                           
         SPACE 1                                                                
         LH    RE,TCOUNT         BUMP COUNTER                                   
         AHI   RE,1                                                             
         STH   RE,TCOUNT                                                        
         SPACE 1                                                                
         MVC   SAVEKEY,KEY       SAVE THE USAGE HISTORY KEY                     
         SPACE 1                                                                
         MVC   TGINV,TLUHINV     MOVE INVOICE TO PRINT LINE                     
         XC    TGINV,HEXFFS                                                     
         GOTO1 TINVCON,DMCB,TGINV,PINV,DATCON                                   
         SPACE 1                                                                
         EDIT  TLUHCSEQ,PSEQ,ALIGN=LEFT  MOVE SEQUENCE# TO PRINT LINE           
         SPACE 1                                                                
         MVC   PUSE,TLUHUSE      MOVE USE TO PRINT LINE                         
         MVC   SVUSE,TLUHUSE                                                    
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         MVC   TGCOM,TLUHCOM                                                    
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',TGCOM)                               
         BE    *+6                                                              
         DC    H'00'             GET COMMERCIAL RECORD                          
         DROP  R3                INTO AIO2                                      
         SPACE 1                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   PAGY,TLCOAGY      MOVE AGENCY TO PRINT LINE                      
         DROP  R4                                                               
         SPACE 1                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   PCID,TACOCID      MOVE COMMERCIAL ID TO PRINT LINE               
         SPACE 1                                                                
         MVC   PMED,TACOMED      MOVE MEDIA TO PRINT LINE                       
         MVC   SVMEDIA,TACOMED   SAVE THE MEDIA                                 
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMTD,R4                                                         
PREP40   L     R4,AIO1           R4=A(USAGE HISTORY RECORD)                     
         MVI   ELCODE,TAMTELQ                                                   
         BAS   RE,GETEL          GET THE CNET/CSYS/MARKET ELEMENTS              
         B     *+8                                                              
PREP50   BAS   RE,NEXTEL                                                        
         BNE   PREP80                                                           
         SPACE 1                                                                
         CLI   TAMTLEN,TAMTULNQ                                                 
         BE    PREP50                                                           
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT  SAVE ELEMENT                                    
         ZIC   RE,TAMTLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),TAMTD                                                 
         MVI   ELEMENT+1,TAMTULNQ                                               
         SPACE 1                                                                
         MVI   TAMTEL,X'FF'      MARK ELEMENT FOR DELETE                        
         SPACE 1                                                                
         MVC   PMES,NTFOUND                                                     
         SPACE 1                                                                
         LHI   RF,TLMTCDQ        READ PRIMARY KEY                               
         MVI   TGMTTYPE,TANPNET                                                 
         CLC   SVUSE,=C'CBL'     IF CABLE                                       
         BE    PREP60                                                           
         CLC   SVUSE,=C'SCB'     SPANISH CABLE                                  
         BE    PREP60                                                           
         MVI   TGMTTYPE,C'S'                                                    
         CLC   SVUSE,=C'LCB'     LOCAL CABLE                                    
         BE    PREP60                                                           
         MVI   TGMTTYPE,C'R'                                                    
         CLI   SVMEDIA,TACOMEDR  OR RADIO WIDLSPOT                              
         BE    PREP60            ELSE READ ALPHA CODE KEY                       
         LHI   RF,TLMTALDQ                                                      
         MVI   TGMTTYPE,C'T'                                                    
PREP60   GOTO1 RECVAL,DMCB,(RF),(X'A4',TAMTCODE)                                
         BNE   PREP70                                                           
         MVC   PMES,SPACES                                                      
         MVC   ELEMENT+TAMTINUM-TAMTD(L'TAMTINUM),TGMTINTC                      
         SPACE 1                                                                
         MVC   PCODE,TAMTCODE    MOVE CNET/CSYS/MKT CODE TO PRT LINE            
         OC    PCODE,SPACES                                                     
         GOTO1 HEXOUT,DMCB,ELEMENT+TAMTINUM-TAMTD,PINT,4                        
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 ADDELEM                                                          
         MVC   AIO,AIO2                                                         
         SPACE 1                                                                
PREP70   GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PREP40                                                           
         SPACE 1                                                                
PREP80   XC    KEY,KEY                  GET USAGE HISTORY RECORD                
         MVC   KEY(L'SAVEKEY),SAVEKEY   INTO AIO2                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'SAVEKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         MVI   TGBYTE,GETUH                                                     
         BAS   RE,PTRACE                                                        
         SPACE 1                                                                
         MVC   AIO,AIO1          PUT USAGE HISTORY                              
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         MVI   TGBYTE,PUTUH                                                     
         BAS   RE,PTRACE         TRACE IF OPTION ON                             
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)   PRINT A BLANK LINE                             
         B     PREP10            AND GO GET NEXT USAGE HISTORY                  
         SPACE 1                                                                
PREP90   GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(25),=CL25'# OF PROCESSED RECORDS:'                           
         EDIT  TCOUNT,(4,P+28)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT RECORD IF TRACE OPTION ON                       
         SPACE 1                                                                
PTRACE   NTR1                                                                   
         CLI   TR,C'Y'                                                          
         BNE   XIT                                                              
         MVI   FORCEHED,C'N'                                                    
         CLI   TGBYTE,GETUH                                                     
         BNE   PTRACE10                                                         
         GOTO1 TRACE,DMCB,AIO,0,=C'GET UH',(0,9)                                
PTRACE10 CLI   TGBYTE,PUTUH                                                     
         BNE   PTRACE20                                                         
         GOTO1 TRACE,DMCB,AIO,0,=C'PUT UH',(0,9)                                
PTRACE20 MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
HEXFFS   DC    12X'FF'                                                          
NTFOUND  DC    C'NOT FOUND'                                                     
GETUH    EQU   X'80'                                                            
PUTUH    EQU   X'40'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H1,53,C'USAGE HISTORY CNET/TMKT/RMKT/CSYS FIX'                   
         SSPEC H2,53,C'-------------------------------------'                   
         SPACE 1                                                                
         SSPEC H7,02,C'AGENCY COMMERCIAL   MEDIA INVOICE SEQ USE'               
         SSPEC H7,44,C'CODE   INTERNAL  MESSAGE'                                
         SSPEC H8,02,C'------ ------------ ----- ------- --- ---'               
         SSPEC H8,44,C'------ --------  -------'                                
         DC    H'0'                                                             
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
TCOUNT   DS    H                   TOTAL COUNT                                  
TR       DS    C                   TRACE OPTION                                 
SVMEDIA  DS    C                   MEDIA                                        
SVUSE    DS    X                   USE                                          
SAVEKEY  DS    CL(L'TLUHKEY)       COUNTER                                      
MYDLNQ   EQU   *-MYD                                                            
*                                                                               
         EJECT                                                                  
*               DSECT FOR PRINT LINE                                            
PRINTD   DSECT                                                                  
         DS    CL1                                                              
PAGY     DS    CL6                 AGENCY                                       
         DS    CL1                                                              
PCID     DS    CL12                COMMERCIAL ID                                
         DS    CL3                                                              
PMED     DS    CL1                 MEDIA                                        
         DS    CL3                                                              
PINV     DS    CL6                 INVOICE                                      
         DS    CL2                                                              
PSEQ     DS    CL2                 SEQUENCE NUMBER                              
         DS    CL2                                                              
PUSE     DS    CL3                 USE                                          
         DS    CL1                                                              
PCODE    DS    CL6                 CODE                                         
         DS    CL1                                                              
PINT     DS    CL8                 INTERNAL CODE                                
         DS    CL2                                                              
PMES     DS    CL9                 MESSAGE                                      
PRINTLNQ EQU   *-PRINTD                                                         
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEAD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065TAREP02   10/08/04'                                      
         END                                                                    
