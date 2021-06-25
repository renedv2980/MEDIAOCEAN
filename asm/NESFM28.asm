*          DATA SET NESFM28    AT LEVEL 022 AS OF 10/31/05                      
*PHASE T31C28A,+0                                                               
         TITLE 'NESFM24 -  DISTRIBUTION RECORD'                                 
         PRINT NOGEN                                                            
T31C28   CSECT                                                                  
         NMOD1 0,T31C28                                                         
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
         BNE   LRCHK                                                            
         BAS   RE,DR                                                            
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
         USING DSTRECD,R4                                                       
*                                                                               
         GOTO1 CLUNPK,DMCB,DSTKCLT,DSTCLT    * CLIENT                           
         OI    DSTCLTH+6,X'80'     XMIT                                         
*                                                                               
         OC    DSTKCODE,DSTKCODE             DIST CODE                          
         BZ    DKEX                                                             
         MVC   DSTCDE,DSTKCODE                                                  
         OI    DSTCDEH+6,X'80'     XMIT                                         
DKEX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
DR       NTR1                                                                   
         L     R4,AIO                                                           
         USING DSTRECD,R4                                                       
         L     R6,AIO                                                           
         USING DSTEL05,R6                                                       
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   DREX                                                             
*                                                                               
         LA    R2,DSTHD1H                                                       
*                                                                               
*  MOVE HEADLINE OUT                                                            
DR100    MVC   8(20,R2),DSTHEAD                                                 
         OI    6(R2),X'80'                                                      
         BAS   RE,NXTFLD                                                        
*                                                                               
*  MOVE NAMES OUT                                                               
         LA    R5,12                                                            
         LA    R7,DSTNAMES                                                      
DR200    OC    0(15,R7),0(R7)                                                   
         BZ    DR250                                                            
         MVC   8(15,R2),0(R7)                                                   
         OI    6(R2),X'80'                                                      
         LA    R7,15(R7)                                                        
DR250    BAS   RE,NXTFLD                                                        
         BCT   R5,DR200                                                         
*                                                                               
         ZIC   RE,1(R6)            NEXT ELEMENT                                 
         AR    R6,RE                                                            
         CLI   0(R6),X'05'                                                      
         BE    DR100               YES DISPLAY                                  
*                                                                               
DREX     B     EXIT                                                             
         DROP  R6,R4                                                            
         EJECT                                                                  
VK       NTR1                      VALIDATE KEY                                 
*                                                                               
         MVI   KEYCOMP,3                                                        
         XC    SVKEY,SVKEY                                                      
         LA    R4,SVKEY                                                         
         USING DSTRECD,R4                                                       
         MVC   DSTKTYPE,=X'0D69'                                                
*                                                                               
         LA    R2,WORK             * MEDIA                                      
         XC    0(8,R2),0(R2)                                                    
         MVI   8(R2),C'N'          SET UP DUMMY HEADER                          
         GOTO1 VALIMED                                                          
         MVC   DSTKAM,BAGYMD                                                    
*                                                                               
         MVI   LISTFLG,0           SET OPTIONAL FLAG                            
         CLI   ACTNUM,ACTLIST      LIST/PRINT OPTIONAL                          
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       LIST/PRINT OPTIONAL                          
         BNE   *+8                                                              
         MVI   LISTFLG,C'Y'        SET OPTIONAL FLAG                            
*                                                                               
         LA    R2,DSTCLTH             * CLIENT                                  
         CLI   5(R2),0                                                          
         BNZ   VK60                                                             
         MVI   ERROR,INVALID                                                    
         CLI   LISTFLG,C'Y'        IS FIELD REQUIRED                            
         BNE   TRAPERR             YES ERROR                                    
         B     VK80                                                             
VK60     MVI   KEYCOMP,5                                                        
         GOTO1 VALIFLD                                                          
         GOTO1 VALICLT                                                          
         MVC   DSTKCLT,BCLT                                                     
*                                                                               
VK80     XC    DSTKCODE,DSTKCODE                                                
         LA    R2,DSTCDEH                                                       
         CLI   5(R2),0                                                          
         BE    VKEX                                                             
         MVC   DSTKCODE(4),8(R2)                                                
         OC    DSTKCODE(4),SPACES                                               
VKEX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
VR       NTR1                                                                   
*                                                                               
         L     R4,AIO                                                           
         USING DSTRECD,R4                                                       
*                                                                               
         MVI   ELEMSEQ,0                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,DELEL                                                         
*                                                                               
         LA    R2,DSTHD1H                                                       
         BAS   RE,BLDELEM                                                       
*                                                                               
         LA    R2,DSTHD2H                                                       
         BAS   RE,BLDELEM                                                       
*                                                                               
         LA    R2,DSTHD3H                                                       
         BAS   RE,BLDELEM                                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* BUILD THE 05 ELEMENTS OFF THE SCREEN FIELDS                                   
*                                                                               
BLDELEM  NTR1                                                                   
         XC    WORK2,WORK2                                                      
         LA    R5,WORK2                                                         
         USING DSTEL05,R5                                                       
         MVI   DSTEL05,X'05'      MAIN ELEMENT                                  
         MVI   DSTELNQ,DST5ELLN   LENGTH                                        
*  KEEP ELEMENTS IN CORRECT SEQUENCE                                            
         ZIC   RE,ELEMSEQ                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,1,ELEMSEQ                                                     
         MVC   DSTSEQ,ELEMSEQ                                                   
*                                                                               
*  MOVE HEADLINE OUT                                                            
         MVC   DSTHEAD,8(R2)                                                    
         OC    DSTHEAD,SPACES                                                   
         BAS   RE,NXTFLD                                                        
*                                                                               
*  MOVE NAMES OUT                                                               
         LA    R6,12                                                            
         LA    R7,DSTNAMES                                                      
BLDE100  CLI   5(R2),0                                                          
         BE    BLDE150                                                          
         MVC   0(15,R7),8(R2)                                                   
         OC    0(15,R7),SPACES                                                  
         LA    R7,15(R7)                                                        
BLDE150  BAS   RE,NXTFLD                                                        
         BCT   R6,BLDE100                                                       
*                                                                               
         OC    DSTNAMES,DSTNAMES   ANY NAMES                                    
         BZ    EXIT                NO EXIT                                      
*                                                                               
         BAS   RE,PUTEL            WRITE ELEMENT OUT                            
         B     EXIT                                                             
         DROP  R5                                                               
         SPACE 3                                                                
NXTFLD   ST    RE,SAVERE                                                        
NXTF20   ZIC   RE,0(2)                                                          
         AR    R2,RE                                                            
         TM    1(R2),X'20'         IS FIELD PROTECTED                           
         BO    NXTF20              BYPASS                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
LR       NTR1                                                                   
*                                                                               
         MVI   NLISTS,X'0F'                                                     
         OC    KEY(13),KEY         IS KEY ALL NULLS?                            
         BNZ   *+10                NO, DO A READ HIGH                           
         MVC   KEY,SVKEY           YES, MOVE IN SAVED KEY                       
         GOTO1 VSETSPT                                                          
LR5      GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
*                                                                               
LR20     DS    0H                                                               
         ZIC   R1,KEYCOMP                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),SVKEY                                                     
         BNE   LRX                                                              
LR30     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING DSTRECD,R4                                                       
*&&DO                                                                           
         MVC   BCLT,DSTKCLT                                                     
         GOTO1 VLMTDACC                                                         
*&&                                                                             
         GOTO1 VLMTDACC,DMCB,DSTKAM,DSTKCLT                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   LR35                                                             
*                                                                               
         XC    KEY,KEY                                                          
         ICM   RF,3,DSTKCLT                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,KEY+3                                                       
         MVC   KEY(3),0(R4)                                                     
         B     LR5                                                              
*                                                                               
LR35     CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR           PREPARE A LIST LINE                          
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         GOTO1 CLUNPK,DMCB,DSTKCLT,LRCLT     * CLIENT                           
         OC    DSTKCODE,DSTKCODE                                                
         BZ    *+10                                                             
         MVC   LRCODE,DSTKCODE                                                  
*                                                                               
*  READ ELEMENTS FOR HEADLINES                                                  
         L     R6,AIO                                                           
         USING DSTEL05,R6                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR40                                                             
         MVC   LRHEAD1,DSTHEAD                                                  
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'05'                                                      
         BNE   LR40                                                             
         MVC   LRHEAD2,DSTHEAD                                                  
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
***************                                                                 
         GOTO1 CLUNPK,DMCB,DSTKCLT,PRCLT     * CLIENT                           
         OC    DSTKCODE,DSTKCODE                                                
         BZ    *+10                                                             
         MVC   PRCODE,DSTKCODE                                                  
*                                                                               
*  READ ELEMENTS FOR HEADLINES                                                  
         L     R6,AIO                                                           
         USING DSTEL05,R6                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR40                                                             
         MVC   PRHEAD1,DSTHEAD                                                  
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'05'                                                      
         BNE   PR40                                                             
         MVC   PRHEAD2,DSTHEAD                                                  
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         CLI   0(R6),X'05'                                                      
         BNE   PR40                                                             
         MVC   PRHEAD3,DSTHEAD                                                  
*                                                                               
PR40     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR10                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R4)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R4),WORK2                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,37,C'NETWORK DISTRIBUTION RECORDS'                            
         SSPEC H2,37,C'----------------------------'                            
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
         MVC   PRCLT(3),=C'CLT'                                                 
         MVC   PRCLT+132(3),=4C'-'                                              
         MVC   PRCODE(4),=C'CODE'                                               
         MVC   PRCODE+132(4),=4C'-'                                             
         MVC   PRHEAD1+5(10),=CL10'HEADLINE 1'                                  
         MVC   PRHEAD1+132(20),=20C'-'                                          
         MVC   PRHEAD2+5(10),=CL10'HEADLINE 2'                                  
         MVC   PRHEAD2+132(20),=20C'-'                                          
         MVC   PRHEAD3+5(10),=CL10'HEADLINE 3'                                  
         MVC   PRHEAD3+132(20),=20C'-'                                          
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
         SPACE 2                                                                
PLINED   DSECT                                                                  
PRCLT    DS    CL3                                                              
         DS    CL3                                                              
PRCODE   DS    CL4                                                              
         DS    CL3                                                              
PRHEAD1  DS    CL20                                                             
         DS    CL3                                                              
PRHEAD2  DS    CL20                                                             
         DS    CL3                                                              
PRHEAD3  DS    CL20                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
LRCLT    DS    CL3                                                              
         DS    CL3                                                              
LRCODE   DS    CL4                                                              
         DS    CL3                                                              
LRHEAD1  DS    CL20                                                             
         DS    CL3                                                              
LRHEAD2  DS    CL20                                                             
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENDIST                                                      
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
       ++INCLUDE NESFMB9D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMBAD                                                       
         EJECT                                                                  
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C28 WORK AREA  *******                  
SAVERE   DS    F                                                                
LISTFLG  DS    CL1                                                              
KEYCOMP  DS    CL1                                                              
ELEMSEQ  DS    XL1                                                              
WORK2    DS    CL250                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022NESFM28   10/31/05'                                      
         END                                                                    
