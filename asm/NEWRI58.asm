*          DATA SET NEWRI58    AT LEVEL 007 AS OF 10/23/08                      
*PHASE T32058A,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE HELLO                                                                  
         TITLE 'T32058 - PROGRAM REC DELETE REPORT'                             
                                                                                
***************************************************************                 
*                                                                               
*        ALLOWS USER TO DELETE ONE OR ALL PROGRAM CODES                         
*        FOR A SPECIFIED NETWORK/DATE RANGE (JUN00 - SCHT)                      
*        REPORT MODULE                                                          
*                                                                               
***************************************************************                 
T32058   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEPRGD,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HEADSPC                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS2          ANETWS2+300=WORKING STORAGE                  
         A     R7,=F'300'                                                       
         USING WORKD,R7                                                         
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,REPMOD                                                        
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
******************************************************************              
* - READ THROUGH PROGRAMS AND BUILD TABLE OF 1 OR ALL PROGRAMS                  
******************************************************************              
REPMOD   NTR1                                                                   
*                                                                               
         L     R0,=F'200000'                                                    
         ST    R0,PROGTBLN                                                      
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         ST    R1,PROGTBL          A(PROGRAM TABLE)                             
*                                                                               
         MVI   ACTELOPT,C'N'       TELL GENCON NO UPDT ACTIV ELEM               
*                                                                               
         LA    R2,SPLPRGH               PROGRAM                                 
         OC    SPLPRG,SPACES                                                    
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   RM50                                                             
*                                                                               
RM20     DS    0H                  GET ALL PROG RECS (BUILD PROGTBL)            
         XC    COUNTER,COUNTER                                                  
         L     R4,PROGTBL                                                       
         USING PROGTBLD,R4                                                      
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING NPGRECD,RF                                                       
*                                                                               
         MVC   KEY(2),=X'0D20'                                                  
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,MKTNUMB                                                  
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     RM25                                                             
*                                                                               
RMSEQ    DS    0H                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
RM25     CLC   KEY(5),KEYSAVE            SAME NETWORK?                          
         BNE   RM40                      NO - DONE                              
*                                                                               
         CLC   SPLPRG(3),=C'ALL'                                                
         BE    RM30                                                             
         LA    RF,KEY                                                           
         CLC   NPGKPROG,SPLPRG                                                  
         BNE   RMSEQ                                                            
*                                                                               
RM30     MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         LA    RF,KEY                                                           
         CLI   NPGKPROG,0                                                       
         BE    RMSEQ                                                            
*                                                                               
         CLC   NPGKEND,PDATEND                                                  
         BH    RMSEQ               PROG DATE IS > THAN REQ. DATE                
*                                                                               
RM35     DS    0H                                                               
         LA    RF,KEY                                                           
         MVC   PROG,NPGKPROG                                                    
         MVC   PROGEND,NPGKEND                                                  
         DROP  RF                                                               
*                                                                               
         L     R2,AIO                                                           
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GETEL                                                         
         BNE   RM37                                                             
*                                                                               
         OC    2(2,R2),2(R2)       ANY START DATE                               
         BZ    *+10                                                             
         MVC   PROGSTRT,2(R2)                                                   
*                                                                               
RM37     LA    R4,PROGTBLQ(R4)                                                  
*                                                                               
         L     RF,COUNTER                                                       
         AHI   RF,1                                                             
         ST    RF,COUNTER                                                       
*                                                                               
         C     RF,=F'18180'        SHOULD BE NO MORE THAN 18180 RECS            
         BL    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         B     RMSEQ                                                            
*                                                                               
RM40     DS    0H                                                               
         XC    FILENAME,FILENAME         YES/RESET TO UNIT FILE                 
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
         MVI   0(R4),X'FF'         DENOTE END OF TABLE                          
         B     RM60                                                             
*                                                                               
*        CHECK IF PRGRAM REC EXISTS                                             
*                                                                               
RM50     DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         OC    SPLPRG,SPACES      NUKPROG HAS SPACES AT END                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),MKTNUMB                                                 
         MVC   KEY+5(6),SPLPRG                                                  
         MVC   KEY+11(2),PDATEND                                                
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
*                                                                               
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         LA    RF,KEY                                                           
         USING NPGRECD,RF                                                       
*                                                                               
         L     R4,PROGTBL                                                       
         MVC   PROG,NPGKPROG                                                    
         MVC   PROGEND,NPGKEND                                                  
         DROP  RF                                                               
*                                                                               
         L     R2,AIO                                                           
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GETEL                                                         
         BNE   RM60                                                             
*                                                                               
         OC    2(2,R2),2(R2)       ANY START DATE                               
         BZ    *+10                                                             
         MVC   PROGSTRT,2(R2)                                                   
*                                                                               
         MVI   PROGTBLQ(R4),X'FF'                                               
*                                                                               
RM60     XC    FILENAME,FILENAME         YES/RESET TO UNIT FILE                 
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
         LA    R2,SPLTESTH        TEST RUN                                      
*                                                                               
         MVI   TESTRUN,C'N'                                                     
         CLI   SPLTEST,C'N'                                                     
         BE    RM100                                                            
         MVI   TESTRUN,C'Y'                                                     
******************************************************************              
*        FINISHED WITH PROGRAMS - NOW PROCESS UNITS                             
*        GO THROUGH ALL UNITS TO SEE WHICH PROGRAMS IN                          
*        PROGTBL HAS UNITS ASSOCIATED WITH THEM                                 
******************************************************************              
RM100    DS    0H                                                               
         LA    R2,KEY                                                           
         USING NURECD,R2                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           UNIT ID                                      
         MVC   KEY+1(1),NBACTAM                                                 
         GOTO1 HIGH                                                             
         B     RM110                                                            
*                                                                               
RM2SEQ   GOTO1 SEQ                                                              
*                                                                               
RM110    DS    0H                                                               
         CLC   KEY(2),KEYSAVE                                                   
         BNE   RM150               FINISHED W/ UNITS - PROCESS PROGTBL          
*                                                                               
         BAS   RE,FINDMTCH         SEE IF THIS UNIT HAS A PROG IN TABLE         
         B     RM2SEQ              PROCESS NEXT UNIT RECORD                     
*                                                                               
******************************************************************              
*        FINISHED W/ UNITS - NOW PROCESS PROGRAMS                               
*        GO THROUGH PROGTBL AND DELETE PROGRAMS                                 
*        WITHOUT ANY UNITS ASSOCIATED WITH THEM                                 
******************************************************************              
RM150    DS    0H                                                               
         L     R4,PROGTBL                                                       
         USING PROGTBLD,R4                                                      
         XC    COUNTER2,COUNTER2                                                
*                                                                               
RM160    DS    0H                                                               
         CLI   0(R4),X'FF'         FINISHED PROCESSING ALL PROGRAMS?            
         BE    RM210                                                            
*                                                                               
         TM    PROGSTAT,PROGNDEL   DOES THIS PROGRAM HAVE UNITS?                
         BO    RM200               YES - BUMP TO NEXT ENTRY                     
*                                                                               
         BAS   RE,PRINTREC                                                      
*                                                                               
         CLI   TESTRUN,C'Y'                                                     
         BE    RM200               TESTRUN - DON'T ACTUALLY DELETE              
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),MKTNUMB                                                 
         MVC   KEY+5(6),PROG                                                    
         MVC   KEY+11(2),PROGEND                                                
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         OI    KEY+13,X'80'        DELETE KEY                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR ',KEY,KEY                       
*                                                                               
         L     R2,AIO                                                           
         USING NPGRECD,R2                                                       
         OI    NPGCNTL,X'80'       DELETE RECORD                                
         GOTO1 PUTREC                                                           
         DROP  R2                                                               
*                                                                               
RM200    DS    0H                                                               
         LA    R4,PROGTBLQ(R4)                                                  
         B     RM160                                                            
*                                                                               
RM210    DS    0H                                                               
         XC    FILENAME,FILENAME         YES/RESET TO UNIT FILE                 
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(10),=CL10'# PROG DEL'                                          
         EDIT  COUNTER2,(10,P+15),ZERO=NOBLANK,ALIGN=LEFT                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(12),=CL12'# PROG TOTAL'                                        
         EDIT  COUNTER,(10,P+15),ZERO=NOBLANK,ALIGN=LEFT                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
RMX      DS    0H                                                               
         LM    R0,R1,PROGTBLN                                                   
         FREEMAIN R,LV=(0),A=(1)                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R2,SPLNETH                                                       
         XIT1  REGS=(R2)                                                        
******************************************************************              
*        PRINT OUT PROGRAM RECORD                                               
******************************************************************              
PRINTREC NTR1                                                                   
         USING PROGTBLD,R4                                                      
*                                                                               
         MVC   P+1(6),PROG                                                      
*                                                                               
         OC    PROGSTRT,PROGSTRT                                                
         BZ    PR10                                                             
         GOTO1 DATCON,DMCB,(2,PROGSTRT),(5,P+10)                                
*                                                                               
PR10     DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,PROGEND),(5,P+20)                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     RF,COUNTER2                                                      
         AHI   RF,1                                                             
         ST    RF,COUNTER2                                                      
*                                                                               
         XIT1                                                                   
******************************************************************              
*        CHECK IF PROGRAM HAS A UNIT                                            
******************************************************************              
FINDMTCH NTR1                                                                   
         L     R4,PROGTBL                                                       
         USING PROGTBLD,R4                                                      
*                                                                               
         LA    R2,KEY                                                           
         USING NURECD,R2                                                        
*                                                                               
FM10     DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    FMX                 DIDN'T FIND A MATCH                          
*                                                                               
         OC    SPLNET,SPACES                                                    
*                                                                               
         CLC   NUKNET,SPLNET       SAME NETWORK?                                
         BNE   FM100               NO - CHECK NEXT ENTRY                        
         CLC   NUKPROG,PROG        SAME PROGRAM?                                
         BNE   FM100               NO - CHECK NEXT ENTRY                        
         CLC   NUKDATE,PROGEND     UNIT DATE > PROGRAM END DATE?                
         BH    FM100               YES - BUMP TO NEXT ENTRY                     
*                                                                               
         OC    PROGSTRT,PROGSTRT   ANY START DATE FOR THIS PROG CODE?           
         BZ    FM50                NO - FOUND A MATCH                           
*                                                                               
         CLC   NUKDATE,PROGSTRT    UNIT DATE WITHIN DATE RANGE OF PROG?         
         BL    FM100               NO - BUMP TO NEXT ENTRY                      
*                                                                               
FM50     DS    0H                  FOUND MATCH                                  
         OI    PROGSTAT,PROGNDEL   THIS PROG HAS A UNIT - DON'T DELETE          
         B     FMX                                                              
*                                                                               
FM100    DS    0H                                                               
         LA    R4,PROGTBLQ(R4)                                                  
         B     FM10                                                             
*                                                                               
FMX      DS    0H                                                               
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
*                                                                               
         EJECT                                                                  
HEADSPC  SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,46,C'PROGRAM RECORD DELETION REPORT'                          
         SSPEC H2,46,C'______________________________'                          
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,1,C'NETWORK'                                                  
         SSPEC H4,1,C'PROGRAM'                                                  
         SSPEC H5,1,C'END DATE'                                                 
         SSPEC H4,99,REPORT                                                     
         SSPEC H5,99,RUN                                                        
         SSPEC H6,120,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HOOK     NTR1                                                                   
         MVC   H3+10(8),SPLNET                                                  
         MVC   H4+10(6),SPLPRG                                                  
         MVC   H5+10(8),SPLEDT                                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
ANSWADD  DC    F'0'                                                             
*                                                                               
IOAREA   DS    CL2000                                                           
*                                                                               
*        WORKING STORAGE                                                        
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
MKTNUMB  DS    H                   NETWORK MARKET NUMBER                        
PDATSTRT DS    CL2                 PROGRAM REC START DATE COMPRESSED            
PDATEND  DS    CL2                 PROGRAM REC END DATE COMPRESSED              
TESTRUN  DS    CL1                                                              
COUNTER2 DS    F                                                                
*                                                                               
PROGTBLN DS    F                                                                
PROGTBL  DS    F                   A(PROGTBL)                                   
*                                                                               
PROGTBLD DSECT                                                                  
PROG     DS    CL6                                                              
PROGSTRT DS    XL2                                                              
PROGEND  DS    XL2                                                              
PROGSTAT DS    XL1                                                              
PROGNDEL EQU   X'01'               DO NOT DELETE / HAS A UNIT                   
PROGTBLQ EQU   *-PROG                                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICAD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NEWRI58   10/23/08'                                      
         END                                                                    
