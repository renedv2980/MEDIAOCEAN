*          DATA SET CTSFM07    AT LEVEL 008 AS OF 05/01/02                      
*PHASE TA0A07A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM07 -- TERMINAL RECORD PURGE                     *         
*                                                                     *         
*  COMMENTS:     PURGES TERMINAL RECORDS FOR A GIVEN LINE-ID/POLL.    *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMF7 (MAINTENANCE)                        *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER POINTER                    *         
*                R3 -- WORK                                           *         
*                R4 -- CTFILE RECORD                                  *         
*                R5 -- WORK                                           *         
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
         TITLE 'TA0A07 - TERMINAL RECORD PURGE'                                 
TA0A07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A07**,RR=R3                                                 
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
         CLI   MODE,VALREC         DO EVERYTHING AT VALREC                      
         BNE   EXIT                                                             
         EJECT                                                                  
         XC    COUNTER,COUNTER                                                  
         XC    MATCHKEY,MATCHKEY                                                
         MVI   MATCHKEY,C'T'                                                    
*                                                                               
         LA    R2,PRNLINEH         LINE-ID                                      
         CLI   PRNLINEH+5,3                                                     
         BL    BADLINE                                                          
         MVC   MATCHKEY+7(4),PRNLINE                                            
         OC    MATCHKEY+7(4),SPACES                                             
*                                                                               
         LA    R2,PRNPOLLH         POLL                                         
         CLI   PRNPOLLH+5,2                                                     
         BNE   BADPOLL                                                          
         MVC   MATCHKEY+11(2),PRNPOLL                                           
*                                                                               
         CLI   ACTNUM,8            TEST ACTION PURGE                            
         BE    PURGE                                                            
*                                                                               
         CLI   ACTNUM,9            TEST ACTION UNPURGE                          
         BE    UNPURGE                                                          
         DC    H'0'                                                             
         EJECT                                                                  
PURGE    XC    KEY,KEY                                                          
         MVC   KEY(13),MATCHKEY                                                 
         GOTO1 HIGH                                                             
         B     PURGE20                                                          
*                                                                               
PURGE10  GOTO1 SEQ                                                              
*                                                                               
PURGE20  L     R4,AIO                                                           
         MVC   SAVEKEY,0(R4)                                                    
         CLC   MATCHKEY,KEY        TEST SAME LINEID/POLL                        
         BNE   PURGEXIT            NO MORE RECORDS                              
*                                                                               
         OI    27(R4),X'80'        MARK FOR DELETION                            
         LH    R1,COUNTER          INCREMENT COUNTER                            
         LA    R1,1(R1)                                                         
         STH   R1,COUNTER                                                       
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             BUILD PASSIVE KEY                            
         MVI   KEY,C'T'                                                         
         MVC   KEY+23(2),2(R6)                                                  
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                PASSIVE ISN'T THERE                          
*                                                                               
         OI    27(R4),X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             RESTORE ORIGINAL READ SEQUENCE               
         MVC   KEY(25),SAVEKEY                                                  
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 READ                                                             
         CLI   DMCB+8,X'02'        MAKE SURE RECORD WAS DELETED                 
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    DMINBTS,X'F7'                                                    
         B     PURGE10                                                          
         EJECT                                                                  
UNPURGE  XC    KEY,KEY                                                          
         MVC   KEY(13),MATCHKEY                                                 
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 HIGH                                                             
         B     UNPURG20                                                         
*                                                                               
UNPURG10 OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
*                                                                               
UNPURG20 NI    DMINBTS,X'F7'                                                    
         L     R4,AIO                                                           
         MVC   SAVEKEY,0(R4)                                                    
         CLC   MATCHKEY,KEY        TEST SAME LINEID/POLL                        
         BNE   UNPRGXIT            NO MORE RECORDS                              
*                                                                               
         TM    27(R4),X'80'        TEST MARKED FOR DELETION                     
         BZ    UNPURG10            NO                                           
*                                                                               
         NI    27(R4),X'7F'        RESET DELETE BIT                             
         LH    R1,COUNTER          INCREMENT COUNTER                            
         LA    R1,1(R1)                                                         
         STH   R1,COUNTER                                                       
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             BUILD PASSIVE KEY                            
         MVI   KEY,C'T'                                                         
         MVC   KEY+23(2),2(R6)                                                  
         OI    DMINBTS,X'08'                                                    
         GOTO1 READ                                                             
         CLI   DMCB+8,X'02'        TEST MARKED FOR DELETION                     
         BE    *+6                                                              
         DC    H'0'                PASSIVE ISN'T THERE                          
*                                                                               
         NI    DMINBTS,X'F7'                                                    
         NI    27(R4),X'7F'        RESET DELETE BIT                             
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             RESTORE ORIGINAL READ SEQUENCE               
         MVC   KEY(25),SAVEKEY                                                  
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    UNPURG10                                                         
         DC    H'0'                                                             
         EJECT                                                                  
PURGEXIT MVC   CONHEAD,=CL60'XXXX TERMINAL RECORDS DELETED'                     
         EDIT  (B2,COUNTER),(4,CONHEAD),ALIGN=LEFT,ZERO=NOBLANK                 
         B     EXIT                                                             
         SPACE 2                                                                
UNPRGXIT MVC   CONHEAD,=CL60'XXXX TERMINAL RECORDS RESTORED'                    
         EDIT  (B2,COUNTER),(4,CONHEAD),ALIGN=LEFT,ZERO=NOBLANK                 
         B     EXIT                                                             
         SPACE 5                                                                
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
       ++INCLUDE CTSFMF7D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD                                                     
         PRINT ON                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
COUNTER  DS    H                                                                
SAVEKEY  DS    XL25                                                             
MATCHKEY DS    XL13                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008CTSFM07   05/01/02'                                      
         END                                                                    
