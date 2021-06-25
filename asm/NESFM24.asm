*          DATA SET NESFM24    AT LEVEL 012 AS OF 10/31/05                      
*          DATA SET NESFM24    AT LEVEL 003 AS OF 12/15/94                      
*PHASE T31C24A,+0                                                               
         TITLE 'NESFM24 -  REASON CODE RECORD'                                  
         PRINT NOGEN                                                            
T31C24   CSECT                                                                  
         NMOD1 0,T31C24                                                         
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
         USING RSNRECD,R4                                                       
*                                                                               
         MVC   RESCDE,RSNKCODE     MOVE REASON CODE                             
         FOUT  RESCDEH                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
DR       NTR1                                                                   
         L     R4,AIO                                                           
         USING RSNRECD,R4                                                       
*                                                                               
         MVC   RESREA,RSNTEXT                                                   
         FOUT  RESREAH                                                          
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
         XC    SVKEY,SVKEY                                                      
         LA    R4,SVKEY                                                         
         USING RSNRECD,R4                                                       
         MVC   RSNKTYPE,=X'0D77'                                                
         MVC   RSNKAGY,AGENCY                                                   
         MVI   RSNKMED,C'N'                                                     
*                                                                               
         MVC   RSNKCODE,SPACES                                                  
         MVC   RSNKCODE(3),RESCDE                                               
         OC    RSNKCODE(3),SPACES                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
VR       NTR1                                                                   
*                                                                               
         L     R4,AIO                                                           
         USING RSNRECD,R4                                                       
*                                                                               
         MVI   RSNEL01,X'01'      MAIN ELEMENT                                  
         MVI   RSNELNQ,52 '        LENGTH                                       
         MVC   RSNTEXT,RESREA                                                   
         OC    RSNTEXT,SPACES      BLANK FILL                                   
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
         CLC   KEY(3),SVKEY                ID/AM                                
         BNE   LRX                                                              
LR30     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING RSNRECD,R4                                                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR           PREPARE A LIST LINE                          
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         MVC   LRCODE,RSNKCODE                                                  
         MVC   LRREASON,RSNTEXT                                                 
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
         MVC   PRCODE,RSNKCODE                                                  
         MVC   PRREASON,RSNTEXT                                                 
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
         SSPEC H1,40,C'NETWORK REASON RECORDS'                                  
         SSPEC H2,40,C'----------------------'                                  
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
         MVC   PRCODE(8),=C'CODE'                                               
         MVC   PRCODE+132(4),=4C'-'                                             
         MVC   PRREASON(11),=C'REASON TEXT'                                     
         MVC   PRCODE+132(50),=50C'-'                                           
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
PRCODE   DS    CL3                                                              
         DS    CL4                                                              
PRREASON DS    CL50                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
LRCODE   DS    CL3                                                              
         DS    CL4                                                              
LRREASON DS    CL50                                                             
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENREAS                                                      
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
       ++INCLUDE NESFMB1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMB2D                                                       
         EJECT                                                                  
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C24 WORK AREA  *******                  
WORK2    DS    CL64                                                             
DBLOCKA  DS    CL256                                                            
DEMOVAL  DS    A                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NESFM24   10/31/05'                                      
         END                                                                    
