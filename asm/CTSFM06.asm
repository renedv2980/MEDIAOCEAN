*          DATA SET CTSFM06    AT LEVEL 018 AS OF 05/01/02                      
*PHASE TA0A06A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM06 -- PRINTER RENAME MAINTENANCE                *         
*                                                                     *         
*  COMMENTS:     MAINTAINS IDINFO AND SYSLIST PRINTER RECORDS         *         
*                ON CTFILE.                                           *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMF6 (MAINTENANCE)                        *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER POINTER                    *         
*                R3 -- WORK                                           *         
*                R4 -- CTFILE RECORD                                  *         
*                R5 -- TABLE OF IDS                                   *         
*                R6 -- GETEL                                          *         
*                R7 -- WORK                                           *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- BASE                                           *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A06 - PRINTER RENAME MAINTENANCE'                            
TA0A06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A06**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO             RELOCATION FACTOR                            
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   EXIT                                                             
         EJECT                                                                  
         XC    COUNTID,COUNTID     CLEAR ACCUMULATORS                           
         XC    COUNTLST,COUNTLST                                                
*                                                                               
         LA    R2,PRNIDH           ID FILTER                                    
         CLI   PRNIDH+5,2                                                       
         BL    BADFILT             MUST BE AT LEAST 2 CHARACTERS                
         MVC   IDFILTER,PRNID                                                   
         OC    IDFILTER,SPACES                                                  
*                                                                               
         LA    R2,PRNLINEH         LINE-ID                                      
         CLI   PRNLINEH+5,3                                                     
         BL    BADLINE                                                          
         MVC   LINEID,PRNLINE                                                   
         OC    LINEID,SPACES                                                    
*                                                                               
         LA    R2,PRNOPOLH         OLD POLL                                     
         CLI   PRNOPOLH+5,2                                                     
         BNE   BADPOLL                                                          
         MVC   OLDPOLL,PRNOPOL                                                  
*                                                                               
         LA    R2,PRNNPOLH         NEW POLL                                     
         CLI   PRNNPOLH+5,2                                                     
         BNE   BADPOLL                                                          
         MVC   NEWPOLL,PRNNPOL                                                  
*                                                                               
         XC    KEY,KEY             READ FIRST ID RECORD                         
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),IDFILTER                                              
         GOTO1 HIGH                                                             
         B     PROC10                                                           
*                                                                               
PROC5    GOTO1 SEQ                                                              
*                                                                               
PROC10   L     R4,AIO                                                           
         MVC   SAVEKEY,0(R4)                                                    
         CLI   0(R4),C'I'                                                       
         BNE   PROCEXIT            NO MORE ID RECORDS                           
*                                                                               
         ZIC   R1,PRNIDH+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   15(0,R4),IDFILTER                                                
         BH    PROCEXIT            NO MORE ID RECS WITH THIS FILTER             
         BE    *+6                                                              
         DC    H'0'                OH BOY, IS THIS FUCKED UP                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PASSIVE,2(R6)       SAVE PASSIVE POINTER JUST IN CASE            
         MVI   PASSFLAG,C'N'       DON'T CHANGE PASSIVE YET                     
*                                                                               
         L     R6,AIO                                                           
         USING CTPRND,R6                                                        
         MVI   ELCODE,X'3A'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROC5               NO PRINTER ELEMENTS                          
*                                                                               
PROC20   OC    CTPRNFLG,CTPRNFLG   TEST IT'S A LIST                             
         BNZ   *+12                NO                                           
         BAS   RE,PROCLIST         YES - CHECK OUT SYSLIST RECORD               
         B     PROC30                                                           
*                                                                               
         CLC   LINEID,CTPRNLIN     TEST MATCH ON LINE-ID                        
         BNE   PROC30                                                           
         CLC   OLDPOLL,CTPRNADD    TEST MATCH ON POLL                           
         BNE   PROC30                                                           
         MVC   CTPRNADD(2),NEWPOLL PUT NEW POLL IN RECORD                       
         MVI   PASSFLAG,C'Y'                                                    
         DROP  R6                                                               
*                                                                               
PROC30   BAS   RE,NEXTEL           CHECK ALL PRINTERS                           
         BE    PROC20                                                           
*                                                                               
         CLI   PASSFLAG,C'Y'       TEST PASSIVE NEEDS TO BE CHANGED             
         BNE   PROC5               NO                                           
*                                                                               
         LH    R1,COUNTID          INCREMENT COUNTER                            
         LA    R1,1(R1)                                                         
         STH   R1,COUNTID                                                       
         GOTO1 WRITE               WRITE RECORD TO FILE                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             BUILD PASSIVE KEY                            
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),PASSIVE                                                
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                PASSIVE ISN'T THERE                          
*                                                                               
         L     R6,AIO                                                           
         USING CTPRND,R6                                                        
         MVI   ELCODE,X'3A'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROC130  OC    CTPRNFLG,CTPRNFLG   TEST IT'S A LIST                             
         BZ    PROC140             YES, WE'VE ALREADY DONE THIS                 
         CLC   LINEID,CTPRNLIN     TEST MATCH ON LINE-ID                        
         BNE   PROC140                                                          
         CLC   OLDPOLL,CTPRNADD    TEST MATCH ON POLL                           
         BNE   PROC140                                                          
         MVC   CTPRNADD(2),NEWPOLL PUT NEW POLL IN RECORD                       
         DROP  R6                                                               
*                                                                               
PROC140  BAS   RE,NEXTEL           CHECK ALL PRINTERS                           
         BE    PROC130                                                          
         GOTO1 WRITE               WRITE PASSIVE RECORD TO FILE                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             RESTORE ORIGINAL READ SEQUENCE               
         MVC   KEY(25),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     PROC5                                                            
         EJECT                                                                  
PROCLIST NTR1                                                                   
*                                                                               
         MVI   LISTFLAG,C'N'                                                    
         XC    KEY,KEY             BUILD SYSLIST KEY                            
         MVI   KEY,C'W'                                                         
         MVI   KEY+17,C'R'                                                      
         MVC   KEY+18(6),4(R6)                                                  
         MVC   AIO,AIO2            USE IO2 FOR SYSLIST RECORD                   
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                SYSLIST RECORD ISN'T THERE                   
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'A4'        PRINTER ELEMENTS                             
         BAS   RE,GETEL                                                         
         BNE   PROCLX              NO ELEMENTS                                  
*                                                                               
PROCL5   CLC   LINEID,3(R6)        TEST MATCH ON LINE-ID                        
         BNE   PROCL10                                                          
         CLC   OLDPOLL,7(R6)       TEST MATCH ON POLL                           
         BNE   PROCL10                                                          
         MVC   7(2,R6),NEWPOLL     PUT NEW POLL IN RECORD                       
         MVI   LISTFLAG,C'Y'                                                    
*                                                                               
PROCL10  BAS   RE,NEXTEL           CHECK ALL PRINTERS                           
         BE    PROCL5                                                           
*                                                                               
         CLI   LISTFLAG,C'Y'       TEST RECORD WAS CHANGED                      
         BNE   PROCLX              NO                                           
*                                                                               
         LH    R1,COUNTLST         INCREMENT COUNTER                            
         LA    R1,1(R1)                                                         
         STH   R1,COUNTLST                                                      
         GOTO1 WRITE               WRITE RECORD TO FILE                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROCLX   XC    KEY,KEY             RESTORE ORIGINAL READ SEQUENCE               
         MVC   KEY(25),SAVEKEY                                                  
         MVC   AIO,AIO3            DUMMY READ                                   
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVI   ELCODE,X'3A'                                                     
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
PROCEXIT MVC   CONHEAD,=CL60'XXXX ID RECORDS UPDATED.   XXXX SYSLIST RE+        
               CORDS UPDATED.'                                                  
         LA    R3,CONHEAD                                                       
         EDIT  (B2,COUNTID),(4,(R3)),ALIGN=LEFT,ZERO=NOBLANK                    
         LA    R3,CONHEAD+27                                                    
         EDIT  (B2,COUNTLST),(4,(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     EXIT                                                             
         SPACE 5                                                                
BADFILT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADFILTM),BADFILTM                                     
         GOTO1 ERREX2                                                           
BADFILTM DC    C'* ERROR * FILTER MUST BE AT LEAST 2 CHARACTERS *'              
*                                                                               
BADLINE  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADLINEM),BADLINEM                                     
         GOTO1 ERREX2                                                           
BADLINEM DC    C'* ERROR * LINE-ID MUST BE 3 OR 4 CHARACTERS *'                 
*                                                                               
BADPOLL  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADPOLLM),BADPOLLM                                     
         GOTO1 ERREX2                                                           
BADPOLLM DC    C'* ERROR * POLL MUST BE EXACTLY 2 CHARACTERS *'                 
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
EXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE CTSFMFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE CTSFMF6D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD                                                     
         PRINT ON                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
COUNTID  DS    H                                                                
COUNTLST DS    H                                                                
SAVEKEY  DS    XL25                                                             
IDFILTER DS    CL10                                                             
LINEID   DS    CL4                                                              
OLDPOLL  DS    CL2                                                              
NEWPOLL  DS    CL2                                                              
PASSIVE  DS    XL2                                                              
PASSFLAG DS    C                                                                
LISTFLAG DS    C                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018CTSFM06   05/01/02'                                      
         END                                                                    
