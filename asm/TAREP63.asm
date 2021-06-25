*          DATA SET TAREP63    AT LEVEL 003 AS OF 04/08/11                      
*PHASE T70363E,*                                                                
         TITLE 'T70363 - CSYS FILE TRANSFER'                                    
T70363   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70363                                                         
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
         BAS   RE,OPENDISK         OPEN DISK                                    
         BAS   RE,PREP             READ DISK,UPDATE W4'S,PRINT REPORT           
         BAS   RE,CLOSDISK         CLOSE DISK                                   
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
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         SPACE 1                                                                
         XC    COUNTS,COUNTS                                                    
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)                                    
         SPACE 1                                                                
         USING TASYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   INTNUM,TASYLMKT                                                  
         DROP  R4                                                               
         SPACE 1                                                                
PREP10   XC    CSYSREC(255),CSYSREC CLEAR AREA FOR CSYS REC FROM DISK           
         BAS   RE,GETCNREC         GET CSYS RECORD FROM DISK                    
         SPACE 1                                                                
         CLI   CSYSREC,X'FF'       IF NO MORE RECORDS ON DISK                   
         BE    PREP20              DONE                                         
         SPACE 1                                                                
         LA    R3,CSYSREC          R3=A(DISK RECORD)                            
         USING CSYSD,R3                                                         
         BAS   RE,GETCSYS          GET CSYS RECORD FROM FILE                    
         BAS   RE,UPDTCSYS         UPDATE CSYS RECORD                           
         BAS   RE,PLINE            PRINT INFORMATION FOR CSYS                   
         BAS   RE,PTRACE           PRINT RECORD IF OPTION ON                    
         B     PREP10              GET NEXT CSYS FROM DISK                      
         SPACE 1                                                                
PREP20   GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0)                                    
         SPACE 1                                                                
         USING TASYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TASYLMKT,INTNUM                                                  
         GOTO1 PUTREC                                                           
         GOTO1 DATAMGR,DMCB,=C'COMMIT'                                          
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(25),=CL25'# OF PROCESSED RECORDS:'                           
         EDIT  TCOUNT,(4,P+28)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(25),=CL25'# OF ADDED RECORDS:'                               
         EDIT  NCOUNT,(4,P+28)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(25),=CL25'# OF CHANGED RECORDS:'                             
         EDIT  ECOUNT,(4,P+28)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO GET CSYS RECORD FROM FILE                             
         SPACE                                                                  
         USING CSYSD,R3            R3=A(CSYS DISK RECORD)                       
GETCSYS  NTR1                                                                   
         LH    RE,TCOUNT           INCREMENT TOTAL COUNTER                      
         AHI   RE,1                                                             
         STH   RE,TCOUNT                                                        
         SPACE 1                                                                
         XC    SVPTRS,SVPTRS                                                    
         MVI   EXIST,C'N'          INITIALIZE EXIST TO NO                       
         SPACE 1                                                                
         L     R2,AIO              CLEAR AIO                                    
         XC    0(255,R2),0(R2)                                                  
         SPACE 1                                                                
         MVC   DUB(4),CSYSCODE                                                  
         MVC   DUB+4(4),ZEROES                                                  
         OC    DUB,SPACES                                                       
         XC    CSYSCODE,CSYSCODE                                                
         SPACE 1                                                                
         CLC   DUB+1(3),SPACES                                                  
         BNE   *+14                                                             
         MVC   DUB+7(1),DUB                                                     
         B     GCSYS10                                                          
         SPACE 1                                                                
         CLC   DUB+2(2),SPACES                                                  
         BNE   *+14                                                             
         MVC   DUB+6(2),DUB                                                     
         B     GCSYS10                                                          
         SPACE 1                                                                
         CLC   DUB+3(1),SPACES                                                  
         BNE   *+14                                                             
         MVC   DUB+5(3),DUB                                                     
         B     GCSYS10                                                          
         SPACE 1                                                                
         MVC   DUB+4(4),DUB                                                     
         SPACE 1                                                                
GCSYS10  MVC   CSYSCODE(4),DUB+4                                                
         OC    CSYSCODE(CSYSLNQ),SPACES                                         
         SPACE 1                                                                
         USING TLMTD,R4                                                         
         LA    R4,KEY                                                           
         XC    TLMTKEY,TLMTKEY     BUILD KEY                                    
         MVI   TLMTCD,TLMTCDQ      AND READ FOR CSYS KEY                        
         MVI   TLMTTYPE,C'S'                                                    
         MVC   TLMTCODE(4),CSYSCODE                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLMTINUM-TLMTD),KEYSAVE                                      
         BNE   GCSYS30                                                          
         SPACE 1                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         GOTO1 ASAVPTRS,DMCB,SVPTRS                                             
         SPACE 1                                                                
         LH    RE,ECOUNT           IF CSYS RECORD EXISTS                        
         AHI   RE,1                INCREMENT EXISTING COUNTER                   
         STH   RE,ECOUNT                                                        
         MVI   EXIST,C'Y'          AND READ CSYS RECORD FOR UPDATE              
         SPACE 1                                                                
         USING PRINTD,R2           R2=A(PRINT LINE)                             
         LA    R2,P                                                             
         SPACE 1                                                                
         USING TANAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GCSYS20                                                          
         ZIC   RE,TANALEN                                                       
         AHI   RE,-3                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PONAME(0),TANANAME                                               
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMSD,R4                                                         
GCSYS20  L     R4,AIO                                                           
         MVI   ELCODE,TAMSELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         EDIT  TAMSWGHT,POSUBS,ALIGN=LEFT,ZERO=NOBLANK                          
         B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 1                                                                
GCSYS30  LH    RE,NCOUNT           IF CSYS RECORD DOES NOT EXIST                
         AHI   RE,1                INCREMENT NEW RECORD COUNTER                 
         STH   RE,NCOUNT           AND INITIALIZE IO AREAD TO KEYSAVE           
         MVC   0(L'TLMTKEY,R2),KEYSAVE                                          
         SPACE 1                                                                
         L     RE,INTNUM                                                        
         AHI   RE,1                                                             
         ST    RE,INTNUM                                                        
         SPACE 1                                                                
         ST    RE,TLMTINUM-TLMTD(R2)                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO UPDATE CSYS RECORD ON FILE                            
         SPACE                                                                  
         USING CSYSD,R3            R3=A(CSYS FILE RECORD)                       
UPDTCSYS NTR1                                                                   
         SPACE 1                                                                
         MVI   ELCODE,TANAELQ      DELETE OLD NAME ELEMENT                      
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         LA    RE,CSYSNAME         DETERMINE LENGTH OF CSYS NAME                
         XR    RF,RF               AND STORE IT IN RF                           
UCSYS10  CLC   0(3,RE),SPACES                                                   
         BE    UCSYS20                                                          
         LA    RE,1(RE)                                                         
         AHI   RF,1                                                             
         B     UCSYS10                                                          
         SPACE 1                                                                
UCSYS20  CHI   RF,L'CSYSNAME                                                    
         BNH   *+8                                                              
         LHI   RF,L'CSYSNAME                                                    
         SPACE 1                                                                
         USING TANAD,R4                                                         
         LA    R4,ELEMENT          BUILD NAME ELEMENT                           
         XC    ELEMENT,ELEMENT     AND ADD IT TO RECORD                         
         MVI   TANAEL,TANAELQ                                                   
         LR    RE,RF                                                            
         AHI   RE,2                                                             
         STC   RE,TANALEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TANANAME(0),CSYSNAME                                             
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         MVI   ELCODE,TAMSELQ      DELETE OLD SUBSCRIBERS ELEMENT               
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         LA    RE,CSYSSUBS         DETERMINE LENGTH OF CSYS SUBSCRIBS           
         XR    RF,RF               AND STORE IT IN RF                           
UCSYS30  CLC   0(3,RE),SPACES                                                   
         BE    UCSYS40                                                          
         LA    RE,1(RE)                                                         
         AHI   RF,1                                                             
         B     UCSYS30                                                          
         SPACE 1                                                                
UCSYS40  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CSYSSUBS(0)                                                  
         CVB   RE,DUB                                                           
         SPACE 1                                                                
         USING TAMSD,R4                                                         
         LA    R4,ELEMENT          BUILD SUBSCRIBERS ELEMENT                    
         XC    ELEMENT,ELEMENT     AND ADD IT TO RECORD                         
         MVI   TAMSEL,TAMSELQ                                                   
         MVI   TAMSLEN,TAMSLNQ                                                  
         ST    RE,TAMSWGHT                                                      
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         CLI   EXIST,C'Y'          IF CSYS RECORD ALREADY EXISTS                
         BNE   UCSYS50                                                          
         GOTO1 PUTREC              PUT IT TO FILE                               
         SPACE 1                                                                
UCSYS50  CLI   EXIST,C'N'          IF CSYS RECORD DOES NOT ALREADY              
         BNE   UCSYSX              EXIST                                        
         GOTO1 ADDREC              ADD IT TO FILE                               
UCSYSX   GOTO1 AADDPTRS,DMCB,SVPTRS                                             
         GOTO1 DATAMGR,DMCB,=C'COMMIT'                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT ONE LINE OF W4 INFO                             
         SPACE 1                                                                
         USING CSYSD,R3            R3=A(CSYS FILE RECORD)                       
PLINE    NTR1                                                                   
         USING PRINTD,R2           R2=A(PRINT LINE)                             
         LA    R2,P                                                             
         MVC   PCODE,CSYSCODE      SYSTEM CODE                                  
         MVC   PNSUBS,CSYSSUBS     SYSTEM SUBS                                  
         MVC   PNNAME(L'CSYSNAME),CSYSNAME     SYSTEM NAME                      
         SPACE 1                                                                
         CLC   POSUBS,PNSUBS       IF SUBS ARE UNCHANGED                        
         BNE   *+10                                                             
         MVC   PNSUBS,SPACES       PRINT SPACES                                 
         SPACE 1                                                                
         CLC   PONAME,PNNAME       IF NAME IS UNCHANGED                         
         BNE   PL10                                                             
         MVC   PNNAME,SPACES       PRINT SPACES                                 
         CLC   PNSUBS,SPACES                                                    
         BNE   PL10                                                             
         MVC   P,SPACES                                                         
         B     XIT                                                              
         SPACE 1                                                                
PL10     GOTO1 SPOOL,DMCB,(R8)     PRINT                                        
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT RECORD IF TRACE OPTION ON                       
         SPACE 1                                                                
PTRACE   NTR1                                                                   
         CLI   TR,C'Y'                                                          
         BNE   XIT                                                              
         MVI   FORCEHED,C'N'                                                    
         CLC   TCOUNT,=H'50'                                                    
         BH    XIT                                                              
         GOTO1 TRACE,DMCB,AIO,0,=C'CSYS RECORD',(0,9)                           
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              DISK ROUTINES                                                    
         SPACE                                                                  
OPENDISK NTR1                                                                   
         L     R2,=A(DISKIN)       OPEN DISK                                    
         OPEN  ((2),INPUT)                                                      
         B     XIT                                                              
         SPACE 1                                                                
CLOSDISK NTR1                                                                   
         L     R2,=A(DISKIN)       CLOSE DISK                                   
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 1                                                                
GETCNREC NTR1                                                                   
         LA    R0,CSYSREC          GET RECORD FROM DISK                         
         L     R1,=A(DISKIN)                                                    
         GET   (1),(0)                                                          
         B     XIT                                                              
         SPACE 1                                                                
DISKEOF  MVI   CSYSREC,X'FF'       SET END OF DISK                              
         B     XIT                                                              
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
ZEROES   DC    C'0000'                                                          
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H1,53,C'CSYS RECORD PROCESSING'                                  
         SSPEC H2,53,C'----------------------'                                  
         SPACE 1                                                                
         SSPEC H7,02,C'CODE  SYS SUBS'                                          
         SSPEC H7,18,C'SYS NAME'                                                
         SSPEC H7,67,C'FILE SUBS'                                               
         SSPEC H7,78,C'FILE NAME'                                               
         SSPEC H8,02,C'----  --------'                                          
         SSPEC H8,18,C'------------------------------------------'              
         SSPEC H8,67,C'---------'                                               
         SSPEC H8,78,C'------------------------------------------'              
         DC    H'0'                                                             
         SPACE 3                                                                
DISKIN   DCB   DDNAME=DISKIN,DSORG=PS,MACRF=(GM),EODAD=DISKEOF,RECFM=VB         
*                                                                               
*&&DO                                                                           
DISKIN   DCB   DDNAME=DISKIN,DSORG=PS,MACRF=(GM),EODAD=DISKEOF,        X        
               RECFM=FB,LRECL=225                                               
*&&                                                                             
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
         DS    0H                                                               
COUNTS   DS    0XL6                                                             
TCOUNT   DS    H                   TOTAL COUNT                                  
ECOUNT   DS    H                   EXISTING COUNT                               
NCOUNT   DS    H                   NEW COUNT                                    
INTNUM   DS    F                   INTERNAL CSYS NUMBER                         
TR       DS    C                                                                
EXIST    DS    X                                                                
CSYSREC  DS    CL250               MINOR DISK RECORD                            
SVPTRS   DS    XL((5*L'TLDRREC)+1)                                              
MYDLNQ   EQU   *-MYD                                                            
*                                                                               
         EJECT                                                                  
*               DSECT FOR CSYS RECORDS                                          
CSYSD    DSECT                                                                  
         DS    XL4                                                              
CSYSCODE DS    CL4                 CABLE SYSTEM CODE                            
         DS    CL6                                                              
CSYSNAME DS    CL50                CABLE SYSTEM NAME                            
CSYSSUBS DS    CL10                CABLE SYSTEM SUBSCRIBERS                     
CSYSLNQ  EQU   *-CSYSD                                                          
         EJECT                                                                  
*               DSECT FOR PRINT LINE                                            
PRINTD   DSECT                                                                  
         DS    CL1                                                              
PCODE    DS    CL4                 CODE                                         
         DS    CL2                                                              
POSUBS   DS    CL9                 OLD SUBSCRIBERS                              
         DS    CL1                                                              
PONAME   DS    CL42                OLD NAME                                     
         DS    CL7                                                              
PNSUBS   DS    CL9                 NEW SUBSCRIBERS                              
         DS    CL2                                                              
PNNAME   DS    CL42                NEW NAME                                     
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
**PAN#1  DC    CL21'003TAREP63   04/08/11'                                      
         END                                                                    
