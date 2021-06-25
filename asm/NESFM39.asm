*          DATA SET NESFM39    AT LEVEL 006 AS OF 10/31/05                      
*PHASE T31C39A,+0                                                               
         TITLE 'NESFM39 -  PROGRAM TYPE RECORD'                                 
         PRINT NOGEN                                                            
T31C39   CSECT                                                                  
         NMOD1 0,T31C39                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASUBSYSD                                                      
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   VKCHK                                                            
         BAS   RE,VK                                                            
         B     RESET                                                            
VKCHK    CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   DKCHK                                                            
         BAS   RE,VR                                                            
         B     RESET                                                            
DKCHK    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   DRCHK                                                            
         BAS   RE,DK                                                            
         B     RESET                                                            
DRCHK    CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   DLCHK                                                            
         BAS   RE,DR                                                            
         B     RESET                                                            
DLCHK    CLI   MODE,RECDEL         DELETE RECORDS                               
         BNE   LRCHK                                                            
         BAS   RE,DL                                                            
         B     RESET                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   PRCHK                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
PRCHK    CLI   MODE,PRINTREP       PRINT RECORDS                                
         BNE   EXIT1                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
*                                                                               
RESET    MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
*                                                                               
EXIT     GOTO1 VSETSPT                                                          
EXIT1    OI    CONSERVH+6,X'01'                                                 
         XIT1                                                                   
         EJECT                                                                  
DK       NTR1                                                                   
         L     R4,AIO                                                           
         USING PTYRECD,R4                                                       
*                                                                               
         MVC   PTYCDE,PTYKCODE     MOVE PROG TYPE CODE                          
         FOUT  PTYCDEH                                                          
*                                                                               
         MVC   PTYSUB,PTYKSUB      MOVE PROG TYPE SUB CODE                      
         FOUT  PTYSUBH                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
DR       NTR1                                                                   
         L     R4,AIO                                                           
         USING PTYRECD,R4                                                       
*                                                                               
         MVC   PTYDES,PTYTEXT                                                   
         FOUT  PTYDESH                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
DL       NTR1                                                                   
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
VK       NTR1                      VALIDATE KEY                                 
*                                                                               
         MVI   NOPTFLG,0           SET OPTIONAL FLAG                            
         CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       LIST/PRINT OPTIONAL                          
         BNE   *+8                                                              
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R4,SVKEY                                                         
         USING PTYRECD,R4                                                       
         MVC   PTYKTYPE,=X'0D54'                                                
         MVC   PTYKAGY,AGENCY                                                   
*                                                                               
         LA    R2,PTYCDEH                                                       
         CLI   PTYCDEH+5,0                                                      
         BNE   VK20                                                             
         CLI   NOPTFLG,1            IS FIELD REQUIRED                           
         BNE   INVERR                                                           
         B     VK40                                                             
VK20     MVC   PTYKCODE(2),PTYCDE                                               
         OC    PTYKCODE(2),SPACES                                               
VK40     CLI   PTYSUBH+5,0                                                      
         BE    VKEX                                                             
         MVC   PTYKSUB(4),PTYSUB                                                
         OC    PTYKSUB(4),SPACES                                                
VKEX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
VR       NTR1                                                                   
*                                                                               
         L     R4,AIO                                                           
         USING PTYRECD,R4                                                       
*                                                                               
         MVI   PTYEL01,X'01'      MAIN ELEMENT                                  
         MVI   PTYELNQ,62          LENGTH                                       
         MVC   PTYTEXT,PTYDES                                                   
         OC    PTYTEXT,SPACES      BLANK FILL                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
LR       NTR1                                                                   
*                                                                               
         MVI   NLISTS,X'0F'                                                     
         OC    KEY(13),KEY         IS KEY ALL NULLS?                            
         BNZ   *+10                NO, DO A READ HIGH                           
         MVC   KEY,SVKEY           YES, MOVE IN SAVED KEY                       
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(4),SVKEY                ID/AM                                
         BNE   LRX                                                              
         CLI   SVKEY+4,0            WAS CODE INPUTTED                           
         BE    LR30                                                             
         CLC   KEY+4(2),SVKEY+4     SEE FOR MATCH ON CODE                       
         BNE   LRX                                                              
         CLI   SVKEY+6,0            WAS CODE INPUTTED                           
         BE    LR30                                                             
         CLC   KEY+6(4),SVKEY+6     SEE FOR MATCH ON SUBCODE                    
         BNE   LRX                                                              
LR30     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING PTYRECD,R4                                                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR           PREPARE A LIST LINE                          
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         MVC   LRCODE,PTYKCODE                                                  
         MVC   LRSUB,PTYKSUB                                                    
         MVC   LRREASON,PTYTEXT                                                 
*                                                                               
LR40     GOTO1 LISTMON                                                          
         B     LR10                GOTO READ SEQ                                
*                                                                               
LRX      B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
PR       DS    0H                  PRINT THE LINE                               
*                                                                               
         MVI   NFILE,C'T'          STATION FILE                                 
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R5,P+10                                                          
         USING PLINED,R5                                                        
*                                                                               
         MVC   PRCODE,PTYKCODE                                                  
         MVC   PRSUB,PTYKSUB                                                    
         MVC   PRREASON,PTYTEXT                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR10                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R7)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R7),ELEM                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,41,C'PROGRAM TYPE RECORDS'                                    
         SSPEC H2,41,C'--------------------'                                    
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
*                                                                               
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PRCODE(4),=C'CODE'                                               
         MVC   PRCODE+132(4),=4C'-'                                             
         MVC   PRSUB(4),=C'SUB'                                                 
         MVC   PRSUB+132(4),=4C'-'                                              
         MVC   PRREASON(11),=C'REASON TEXT'                                     
         MVC   PRREASON+132(60),=60C'-'                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
BADDEM   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DEMOMSG),DEMOMSG                                       
         LR    R2,R4               POINT TO OFFENDING FIELD                     
         GOTO1 ERREX2                                                           
*                                                                               
DEMOMSG  DC    C'INVALID DEMOGRAPHIC CATEGORY'                                  
*                                                                               
DUPDEM   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DUPMSG),DUPMSG                                         
         LR    R2,R4               POINT TO OFFENDING FIELD                     
         GOTO1 ERREX2                                                           
*                                                                               
DUPMSG   DC    C'DUPLICATE DEMOGRAPHIC CATEGORY'                                
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
PLINED   DSECT                                                                  
PRCODE   DS    CL2                                                              
         DS    CL4                                                              
PRSUB    DS    CL4                                                              
         DS    CL2                                                              
PRREASON DS    CL60                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
LRCODE   DS    CL2                                                              
         DS    CL4                                                              
LRSUB    DS    CL4                                                              
         DS    CL2                                                              
LRREASON DS    CL58                                                             
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENPTYP                                                      
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMAAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMABD                                                       
         EJECT                                                                  
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C39 WORK AREA  *******                  
WORK2    DS    CL64                                                             
DBLOCKA  DS    CL256                                                            
DEMOVAL  DS    A                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NESFM39   10/31/05'                                      
         END                                                                    
