*          DATA SET DDPAPTENQ  AT LEVEL 002 AS OF 05/26/10                      
*PHASE PAPTENQA                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'DDPAPTENQ - SHOW ENQ STATUS OF A MOVE REQUEST'                  
***********************************************************************         
*                                                                     *         
* WHEN A PROGRAMMER TRIES TO DO SOMETHING WITH A PANAPT MOVE REQUEST, *         
* AND THE MOVE REQUEST IS EXCLUSIVELY ENQUEUED BY ANOTHER JOB, PANAPT *         
* PRODUCES A REALLY LAME MESSAGE SIMILAR TO THIS:                     *         
*                                                                     *         
* "SOMEONE ELSE IS CURRENTLY ALTERING THIS RECORD. PLEASE TRY AGAIN   *         
*  LATER."                                                            *         
*                                                                     *         
* UNTIL AND UNLESS CA ENHANCES THESE MESSAGES TO INCLUDE THE JOBNAME  *         
* THAT HAS THE ENQ, WE HAVE THIS LITTLE UTILITY PROGRAM THAT WILL     *         
* PROVIDE THE JOBNAME. THIS IS BASED ON THE PANAPT ADMINISTRATOR      *         
* GUIDE VERSION 3.1, APPENDIX F: ENQUEUES. THIS DOCUMENT TELLS US     *         
* WHAT WE NEED TO INVOKE THE ISGQUERY MACRO TO FIND OUT WHO OWNS THE  *         
* MOVE REQUEST.                                                       *         
*                                                                     *         
* THIS PROGRAM IS INVOKED VIA A REXX ROUTINE THAT PROMPTS THE USER    *         
* FOR THE MOVE REQUEST NUMBER. THE PANAPT DATABASE NAME DEFAULTS TO   *         
* THE PRODUCTION DSN 'PANAPT.DDS.APTDB'. THIS MAY BE OVERRIDDEN WITH  *         
* AN APTDB= PARAMETER VIA R1 (FROM THE REXX ROUTINE). THIS IS USEFUL  *         
* FOR TESTING IN THE SANDBOX.                                         *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
PAPTENQ  CSECT                                                                  
         NBASE 0,*PAPTENQ,=V(REGSAVE)                                           
*                                                                               
         LR    R1,RC                                                            
         SHI   R1,4                                                             
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM PASSED FROM REXX ROUTINE)             
         CLC   0(2,R1),=H'9'       MR=NNNNNN                                    
         BNL   *+6                                                              
         DC    H'0'                INVALID PARM                                 
         CLC   =C'MR=',2(R1)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   R_MR#,5(R1)         MOVE REQUEST NUMBER                          
*                                                                               
         CLC   0(2,R1),=H'9'       ANY MORE PARAMETERS?                         
         BNE   *+14                YES                                          
         MVC   R_DBNAME,=CL44'PANAPT.DDS.APTDB' DEFAULT TO PRODUCTION           
         B     QUERYENQ                                                         
*                                                                               
         LA    RE,9+2(R1)          RE = A(NEXT PARAMETER)                       
         CLC   =C',APTDB=',0(RE)   PANAPT DATABASE CLUSTER PARAMETER?           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   R_DBNAME,7(RE)      YES: SET CLUSTER NAME                        
*                                                                               
QUERYENQ DS    0H                                                               
         ISGQUERY REQINFO=QSCAN,                                       +        
               SCANACTION=START,                                       +        
               ANSAREA=ANS_AREA,                                       +        
               ANSLEN=AREA_LEN,                                        +        
               ANSDETAIL=FULL,                                         +        
               SEARCH=BY_FILTER,                                       +        
               QNAMEMATCH=SPECIFIC,                                    +        
               QNAME=Q_NAME,                                           +        
               RNAMEMATCH=PATTERN,                                     +        
               RNAME=R_NAME,                                           +        
               RNAMELEN=RNAMELEN                                                
*                                                                               
         CHI   RF,0                SUCCESSFUL RETURN?                           
         BE    SHOWENQ             YES: DIG OUT THE INFO                        
*                                                                               
         CHI   RF,4                NON-FATAL WARNING?                           
         BE    CHKWRN10            YES                                          
         TPUT  MSGFATAL,L'MSGFATAL,EDIT   FATAL ERROR                           
         B     DONE                                                             
*                                                                               
CHKWRN10 DS    0H                                                               
         LR    R3,R0               ISOLATE THE REASON CODE INTO R3              
         N     R3,=X'0000FFFF'                                                  
         CHI   R3,X'0404'          NO ENQS FOUND?                               
         BE    *+6                 CORRECT                                      
         DC    H'0'                UNACCEPTABLE REASON CODE                     
         TPUT  MSGNOENQ,L'MSGNOENQ,EDIT                                         
         B     DONE                                                             
         EJECT                                                                  
SHOWENQ  DS    0H                                                               
         LA    R4,ANS_AREA         A(ANSWER AREA FROM ISGQUERY)                 
         USING ISGYQUAAHDR,R4                                                   
         L     R4,ISGYQUAAHDRFIRSTRECORD31 (FIRST RESOURCE DATA RECORD)         
         USING ISGYQUAARS,R4                                                    
         L     R4,ISGYQUAARSFIRSTRQ31  A(FIRST REQUESTER DATA RECORD)           
         USING ISGYQUAARQ,R4                                                    
         L     R4,ISGYQUAARQRQX31  A(REQUESTER DATA RECORD EXTENSION)           
         USING ISGYQUAARQX,R4                                                   
         MVC   MSGJOBNM,ISGYQUAARQXJOBNAME    OWNING JOB NAME                   
         MVC   MSGSYS,ISGYQUAARQXSYSNAME      OWNING SYSTEM NAME                
         TPUT  MSGGOT1,MSGGOT1L,EDIT                                            
         DROP  R4                                                               
*                                                                               
DONE     DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
MSGNOENQ DC    C'NO ENQUEUE FOUND FOR THIS MOVE REQUEST.'                       
MSGFATAL DC    C'* FATAL RETURN CODE FROM ISGQUERY MACRO. CONTACT DEIS.+        
                *'                                                              
MSGGOT1  DC    C'JOB '                                                          
MSGJOBNM DS    CL8                 JOB NAME THAT OWNS THE MOVE REQUEST          
         DC    C' ON '                                                          
MSGSYS   DS    CL8                 SYSTEM THAT OWNS THE MOVE REQUEST            
         DC    C' HAS AN ENQUEUE ON THIS MOVE REQUEST.'                         
MSGGOT1L EQU   *-MSGGOT1                                                        
*                                                                               
Q_NAME   DC    CL8'APTSYS'         PANAPT ALWAYS USES THIS MAJOR NAME           
*                                                                               
R_NAME   DS    0CL84                                                            
         ORG   R_NAME                                                           
R_DBNAME DC    CL44' '             PANAPT DATABASE CLUSTER NAME                 
         DC    CL8'APTPEND'        PENDING FILE ENQ                             
R_MR#    DS    CL6                 MOVE REQUEST NUMBER                          
         DC    C'*'                PATTERN WILDCARD                             
RNAME_LQ EQU   *-R_NAME                                                         
         ORG                                                                    
RNAMELEN DC    AL1(RNAME_LQ)       L'RNAME FILTER                               
*                                                                               
AREA_LEN DC    A(AREALENQ)         A(ANSWER AREA)                               
         DS    0L                                                               
         DC    C'***ANSWERAREA***'                                              
ANS_AREA DS    (AREALENQ)X         ANSWER AREA                                  
AREALENQ EQU   4096                ANSWER AREA LENGTH                           
         EJECT                                                                  
         PRINT GEN                                                              
         ISGYQUAA                                                               
         PRINT NOGEN                                                            
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDPAPTENQ 05/26/10'                                      
         END                                                                    
