*          DATA SET PPINF20    AT LEVEL 037 AS OF 05/01/02                      
*PHASE T41A20A,+0,NOAUTO   ***** NOTE - A APPENDED TO PHASE NAME  L01           
         TITLE 'T41A20   CHANGE LOG         '                                   
*                                                                               
*  SMYE 04/19/02 MAKE PUBEDIT CORE-RESIDENT                                     
*                                                                               
*  KWAN 05/99    CORRECT FILTER ERROR DISPLAY                                   
*                                                                               
*  BPLA 7/95     IF REP IS USED AS A PUBLISHER                                  
*                DISPLAY A "P" AFTER THE CODE                                   
*                ALSO IF RECORD IS PUBLISHER DISPLAY ONLY PUBLISHERS            
*                ANOTHER NEW RECORD TYPE - PPUBLIST FOR LISTING                 
*                PUBS FOR A PUBLISHER                                           
*                                                                               
*  ROSA 6/20/88  ADD PUB LIST FEATURE                             L01           
*                                                                 L01           
         TITLE 'T41A20   PRINTPAK INFO  REPS/PUBLISTS/PPUBLIST'                 
T41A20   CSECT                                                                  
         NMOD1 0,T41A20,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING FLDHDRD,R2                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T41AFFD,RA                                                       
         LA    R2,SINHDRH          BUILD HEADLINES                              
         LA    RE,REC2             CLEAR SAVE AREA                              
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     RE,APUBIO           CLEAR PUBREC                                 
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
**********                                                                      
***      THIS PHASE HANDLES VARIOUS RECORD TYPES                                
***                                                                             
***      SVREC= X'16'     PUBLIST RECORDS                                       
***           = X'20'     REP RECORDS                                           
***           = X'22'     PUBLISHERS (TYPE OF REP)                              
***           = X'23'     PUBLISHER PUB LIST                                    
**********                                                                      
*                                                                               
*                                                                               
*                                                                               
         CLI   SINIFLTH+5,0        NONE OF THE ABOVE HAVE FLT FLD               
         BE    *+16                                                             
         LA    R2,SINIFLTH                                                      
         LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
*                                                                               
*                                                                               
         CLI   SVREC,X'16'         PUB LIST REQUEST                             
         BE    PUBLIST                                                          
*                                                                               
         CLI   SVREC,X'23'         PUBLISHER PUB LIST                           
         BE    PPUBLIST                                                         
*                                                                               
         MVC   FLDDATA+1(13),=C'REP CODE/NAME'                                  
         MVC   FLDDATA+39(13),FLDDATA+1                                         
         CLI   SVREC,X'20'          SEE IF REP REQUEST                          
         BE    REP3                                                             
*                                   MUST BE PUBLISHER (SVREC=X'22')             
         MVC   FLDDATA+1(19),=C'PUBLISHER CODE/NAME'                            
         MVC   FLDDATA+39(19),FLDDATA+1                                         
REP3     FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(13),DASH                                               
         MVC   FLDDATA+39(13),DASH                                              
         CLI   SVREC,X'20'          SEE IF REP REQUEST                          
         BE    REP4                                                             
*                                   MUST BE PUBLISHER (SVREC=X'22')             
         MVC   FLDDATA+1(19),DASH                                               
         MVC   FLDDATA+39(19),DASH                                              
REP4     DS    0H                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
*                                                                               
         LA    R6,REC2                                                          
         LA    R7,28                                                            
         LA    R5,KEY              BUILD FIRST KEY                              
         USING REPHDRD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         OC    PREVKEY,PREVKEY     FIRST TIME                                   
         BZ    *+10                                                             
         MVC   KEY,PREVKEY          NO - RESTORE PREV KEY                       
         XC    PREVKEY,PREVKEY                                                  
RHI      BAS   RE,HIGH                                                          
         B     HAVREC                                                           
*                                                                               
RSEQ     BAS   RE,SEQ                                                           
HAVREC   LA    R5,KEY                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BNE   REND                                                             
         BAS   RE,GETREC                                                        
         L     R5,AREC             BUILD TABLE FOR ALPHA LIST                   
*                                                                               
         CLI   SVREC,X'22'         SEE IF PUBLISHER REPORT                      
         BNE   HAVREC5                                                          
         TM    PREPSTAT,X'01'                                                   
         BNO   RSEQ                                                             
*                                   MUST BE PUBLISHER (SVREC=X'22')             
HAVREC5  MVC   0(5,R6),PREPKREP        CODE AND SUFFIX                          
         MVC   5(30,R6),PREPNAME                                                
         MVC   35(1,R6),PREPSTAT      ALSO SAVE STATUS BYTE                     
         LA    R6,36(R6)                                                        
         BCT   R7,RSEQ                                                          
         MVC   PREVKEY,KEY         SAVE KEY FOR NEXT READ                       
         MVI   PREVKEY+8,X'FF'                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
* END OF REP HEADERS SO FORMAT SCREEN                                           
REND     CLI   REC2,0              ANY DATA                                     
         BNE   FORMAT                                                           
         B     FRMTEND                                                          
*                                                                               
FORMAT   XC    DMWORK(20),DMWORK                                                
         LA    R6,REC2                                                          
         LA    R7,0                                                             
FORMAT1  CLI   0(R6),0             COUNT NUMBER OF ENTRIES                      
         BE    FORMAT2                                                          
         LA    R6,36(R6)                                                        
         LA    R7,1(R7)                                                         
         B     FORMAT1                                                          
*                                                                               
FORMAT2  GOTO1 FRMTALPH,DMCB,(36,REC2),(R7),14,(2,DMWORK)                       
FORMAT3  LA    R6,DMWORK                                                        
         LA    RF,FLDDATA+1                                                     
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
FORMAT4  CLI   0(R6),0                                                          
         BE    FRMTSEND                                                         
         L     R7,0(R6)                                                         
         MVC   0(4,RF),0(R7)       MOVE DATA TO SCREEN LINE                     
         CLI   4(R7),0             CHK FOR SUFFIX                               
         BNH   FORMAT5                                                          
         MVI   4(RF),C'.'                                                       
         MVC   5(1,RF),4(R7)                                                    
*                                                                               
         CLI   SVREC,X'22'       SEE IF PUBLISHER REPORT                        
         BE    FORMAT4W                                                         
         TM    35(R7),X'01'       SEE IF PUBLISHER                              
         BO    FORMAT4P                                                         
FORMAT4C MVI   6(RF),C'/'                                                       
         MVC   7(30,RF),5(R7)                                                   
         B     FORMAT7                                                          
*                                                                               
FORMAT4P DS    0H                                                               
         MVI   6(RF),C'P'                                                       
FORMAT4Q MVI   7(RF),C'/'                                                       
         MVC   8(30,RF),5(R7)                                                   
         B     FORMAT7                                                          
*                                                                               
FORMAT4W DS    0H                   IF PUBLISHER REPORT                         
         TM    35(R7),X'02'                                                     
         BNO   FORMAT4C                                                         
         MVI   6(RF),C'R'           I/O REPEAT CHECK                            
         B     FORMAT4Q                                                         
*                                                                               
FORMAT5  DS    0H                                                               
*                                                                               
         CLI   SVREC,X'22'       SEE IF PUBLISHER REPORT                        
         BE    FORMAT5W                                                         
         TM    35(R7),X'01'       SEE IF PUBLISHER                              
         BO    FORMAT5P                                                         
*                                                                               
FORMAT5C DS    0H                                                               
         MVI   4(RF),C'/'                                                       
         MVC   5(30,RF),5(R7)                                                   
         B     FORMAT7                                                          
*                                                                               
FORMAT5P DS    0H                                                               
         MVI   4(RF),C'P'                                                       
FORMAT5Q MVI   5(RF),C'/'                                                       
         MVC   6(30,RF),5(R7)                                                   
         B     FORMAT7                                                          
*                                                                               
FORMAT5W DS    0H                   IF PUBLISHER REPORT                         
         TM    35(R7),X'02'                                                     
         BNO   FORMAT5C                                                         
         MVI   4(RF),C'R'           I/O REPEAT CHECK                            
         B     FORMAT5Q                                                         
*                                                                               
FORMAT7  SR    RE,RE               DECREMENT COUNT                              
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         LA    R5,36(R5)                                                        
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)            NEXT COLUMN                                  
         LA    RF,38(RF)                                                        
         B     FORMAT4                                                          
*                                                                               
FRMTSEND FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3                                                          
*                                                                               
FRMTEND  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT  OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         SPACE 2                                                                
LINLEN   EQU   88                                                               
DASH     DC    40C'-'                                                           
SPACES   DC    40C' '                                                           
         EJECT                                                                  
PUBLIST  MVC   FLDDATA+1(32),=C'LIST CODE/ NAME / NUMBER OF PUBS'               
         MVC   FLDDATA+38(33),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(33),DASH                                               
         MVC   FLDDATA+38(33),DASH                                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
*                                                                               
         LA    R6,REC2                                                          
         LA    R7,28                                                            
         LA    R5,KEY              BUILD FIRST KEY                              
         USING REPHDRD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         OC    PREVKEY,PREVKEY     FIRST TIME                                   
         BZ    *+10                                                             
         MVC   KEY,PREVKEY          NO - RESTORE PREV KEY                       
         XC    PREVKEY,PREVKEY                                                  
RHIA     BAS   RE,HIGH                                                          
         B     HAVRECA                                                          
*                                                                               
RSEQA    BAS   RE,SEQ                                                           
HAVRECA  LA    R5,KEY                                                           
         CLC   KEY(7),SVKEY    IF AGY/MED/ID/CLI CHANGES                        
         BNE   RENDA                                                            
         BAS   RE,GETREC                                                        
         L     R5,AREC             BUILD TABLE FOR ALPHA LIST                   
         MVC   0(20,R6),PLISDESC       LIST NAME                                
         OC    0(20,R6),SPACES         IN CASE ALL ZEROS                        
         MVC   20(2,R6),PLISNPBS                                                
         MVC   22(3,R6),PLISKCOD                                                
         LA    R6,35(R6)                                                        
BKUP     BAS   RE,SEQ                                                           
         CLC   KEY(10),KEYSAVE  MAY HAVE MORE THAN 1 RECORD FOR LIS             
         BNE   NOTSAME          NAME                                            
         SR    RE,RE                                                            
         ICM   RE,3,PLISNPBS    LOAD NUMBER OF PUBS FOR SECOND                  
         LR    RF,R6            WITH SAME CODE // MUST ACCUMULATE               
         SH    RF,=H'35'                                                        
         AH    RE,20(RF)                                                        
         STH   RE,20(RF)                                                        
         B     BKUP                                                             
NOTSAME  MVC   KEYSAVE,KEY                                                      
         BCT   R7,HAVRECA                                                       
         MVC   PREVKEY,KEY         SAVE KEY FOR NEXT READ                       
         MVI   PREVKEY+10,X'FF'                                                 
         B     *+10                                                             
*                                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
RENDA    XC    PREVKEY,PREVKEY    FORCE MESSAGE IN BASE                         
*                                * END OF REQUESTED DATA *                      
* END OF REP HEADERS SO FORMAT SCREEN                                           
         CLI   REC2,0              ANY DATA                                     
         BNE   FORMATA                                                          
         B     FRMTENDA                                                         
         EJECT                                                                  
PPUBLIST DS    0H           DISPLAY PUBS FOR A PUBLISHER                        
         MVC   FLDDATA+1(13),=C'PUB CODE/NAME'                                  
         MVC   FLDDATA+38(13),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(13),DASH                                               
         MVC   FLDDATA+38(13),DASH                                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
*                                                                               
         LA    R6,REC2                                                          
         LA    R7,28                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'0P'       PUBLISHER/PUB POINTERS                       
         MVC   KEY+2(3),SVKEY      AGY/MED                                      
         MVC   KEY+5(5),SVKEY+4    PUBLISHER (REP)                              
         OC    PREVKEY,PREVKEY     FIRST TIME                                   
         BZ    *+10                                                             
         MVC   KEY,PREVKEY          NO - RESTORE PREV KEY                       
         XC    PREVKEY,PREVKEY                                                  
RHIP     BAS   RE,HIGHPUB                                                       
         B     HAVRECP                                                          
*                                                                               
RSEQP    BAS   RE,SEQPUB                                                        
HAVRECP  DS    0H                                                               
         CLC   KEY(10),KEYSAVE    SEE IF RIGHT PUBLISHER                        
         BNE   PRENDA                                                           
         BAS   RE,GETPUB                                                        
         L     R5,APUBIO           BUILD TABLE FOR ALPHA LIST                   
         USING PUBRECD,R5                                                       
         MVC   0(20,R6),PUBNAME        LIST NAME                                
         MVC   20(6,R6),PUBKPUB                                                 
         LA    R6,35(R6)                                                        
         MVC   PREVKEY,KEY                                                      
         BCT   R7,RSEQP                                                         
         B     PRENDB                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
PRENDA   XC    PREVKEY,PREVKEY    FORCE MESSAGE IN BASE                         
*                                * END OF REQUESTED DATA *                      
* END OF RRECORDS  SO FORMAT SCREEN                                             
PRENDB   CLI   REC2,0              ANY DATA                                     
         BNE   FORMATA                                                          
         B     FRMTENDA                                                         
         EJECT                                                                  
*                                                                               
FORMATA  XC    DMWORK(20),DMWORK                                                
         LA    R6,REC2                                                          
         LA    R7,0                                                             
FORMAT1A CLI   0(R6),0             COUNT NUMBER OF ENTRIES                      
         BE    FORMAT2A                                                         
         LA    R6,35(R6)                                                        
         LA    R7,1(R7)                                                         
         B     FORMAT1A                                                         
*                                                                               
FORMAT2A GOTO1 FRMTALPH,DMCB,(35,REC2),(R7),14,(2,DMWORK)                       
FORMAT3A LA    R6,DMWORK                                                        
         LA    RF,FLDDATA+1                                                     
         CLI   0(R6),0                                                          
         BE    FRMTENDA                                                         
FORMAT4A CLI   0(R6),0                                                          
         BE    FRMTSENA                                                         
         L     R7,0(R6)                                                         
         CLI   SVREC,X'23'        SEE IF DOING PUBLISHER PUB LIST               
         BE    FORMAT5A                                                         
*                                                                               
         MVI   0(RF),C'('                                                       
         MVC   1(3,RF),22(R7)                                                   
         MVI   4(RF),C')'                                                       
         MVC   6(20,RF),0(R7)      MOVE DATA TO SCREEN LINE                     
         SR    RE,RE                                                            
         ICM   RE,3,20(R7)                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,15                                                         
         MVC   27(6,RF),=X'402020202020'                                        
         ED    27(6,RF),DUB+5                                                   
         B     FORMAT7A                                                         
*                                                                               
FORMAT5A DS    0H                 PUBLISHER PUB DISPLAY                         
         ST    RF,FULL                                                          
         XC    WORK(20),WORK                                                    
*NOP*    GOTO1 =V(PUBEDIT),DMCB,20(R7),WORK,RR=RELO                             
         GOTO1 VPUBEDIT,DMCB,20(R7),WORK                                        
         L     RF,FULL                                                          
         MVC   0(15,RF),WORK                                                    
         LA    R1,15(RF)                                                        
FMT5A2   CLI   0(R1),C' '                                                       
         BH    FMT5A4                                                           
         BCTR  R1,0                                                             
         B     FMT5A2                                                           
*                                                                               
FMT5A4   DS    0H                 FLOAT NAME AFTER NUMBER                       
         MVI   1(R1),C'/'                                                       
         MVC   2(20,R1),0(R7)                                                   
*                                                                               
FORMAT7A SR    RE,RE               DECREMENT COUNT                              
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         LA    R5,35(R5)                                                        
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)            NEXT COLUMN                                  
         LA    RF,37(RF)                                                        
         B     FORMAT4A                                                         
*                                                                               
FRMTSENA FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3A                                                         
*                                                                               
FRMTENDA LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXITA OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
       ++INCLUDE PPGENEROL                                                      
         LTORG                                                                  
*                                                                               
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
*                                                                               
REPHDRD  DSECT                                                                  
       ++INCLUDE PREPREC                                                        
         ORG   REPHDRD                                                          
       ++INCLUDE PLISREC                                                        
       ++INCLUDE PLISDTEL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSINFOWRK                                                     
