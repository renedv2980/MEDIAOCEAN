*          DATA SET YYUNJMC    AT LEVEL 023 AS OF 05/01/02                      
*PHASE YYUNJMCA                                                                 
INV1     TITLE 'CLASSIFY INVENTORY FOR PANAPT'                                  
JMCINV1  CSECT                                                                  
R0       EQU   0                                                                
R1       EQU   1                                                                
R2       EQU   2                                                                
R3       EQU   3                        LOOP CONTROL                            
R4       EQU   4                        LOOP CONTROL                            
R5       EQU   5                        UNUSED                                  
R6       EQU   6                        UNUSED                                  
R7       EQU   7                        WORK                                    
R8       EQU   8                        WORK                                    
R9       EQU   9                        WORK                                    
R10      EQU   10                       WORK - ITERATION COUNTER                
R11      EQU   11                       RESERVED                                
R12      EQU   12                       PROGRAM BASE                            
R13      EQU   13                       WORKING STORAGE BASE                    
R14      EQU   14                       SUBROUTINE LINK REG                     
R15      EQU   15                       RETURN CODES                            
         SPACE 5                                                                
         USING *,R12                                                            
         STM   R14,R12,12(R13)                                                  
         LA    R12,0(R15)                                                       
         LA    R15,SAVE                                                         
         ST    R15,8(R13)                                                       
         ST    R13,4(R15)                                                       
         LR    R13,R15                                                          
         USING SAVE,R13                                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*   THIS SECTION INITIALIZES SWITCHES, FLAGS, AND POINTERS.           *         
*   IT ALSO INITIALIZES THE PRIMARY DDCB'S AND PACB'S.                *         
*                                                                     *         
***********************************************************************         
INIT00   EQU   *                                                                
         XC    RTRNCD,RTRNCD            POSIT MEMBER FOUND                      
         OPEN  (INVIN,INPUT)                                                    
         OPEN  (INVOUT,OUTPUT)                                                  
         OPEN  (SYSPRINT,OUTPUT)                                                
         OPEN  (ERRPRINT,OUTPUT)                                                
         BAS   R14,POPEN          ISSUE PAM OPEN                                
   SPACE                                                                        
         LTR   R15,R15            TEST RETURN CODE                              
         BZ    MAIN               OK; CONTINUE                                  
   SPACE                                                                        
INITERR  EQU   *                                                                
         ST    R15,RTRNCD                                                       
         MVC   OUTREC,BLANKS                                                    
         ST    R15,OUTREC+4                                                     
         PUT   ERRPRINT,OUTREC                                                  
         B     EXIT                                                             
   EJECT                                                                        
***********************************************************************         
*                                                                     *         
*   THIS IS THE MAINLINE.                                             *         
*                                                                     *         
***********************************************************************         
*                                                                               
*   BEGIN PROCESSING                                                            
*                                                                               
MAIN     EQU   *                                                                
         GET   INVIN,INREC                                                      
         MVC   PACNAME,INNAME                                                   
*                                                                               
         LA    R3,2                      SET MAX ITERATIONS                     
GETREC   EQU   *                                                                
         BAS   R14,PREAD                 GET A RECORD FROM THE MEMBER           
*                                                                               
         CLI   RECORD,C'S'               SCREEN CHAR?                           
         BNE   NXTREC                    NO, GET NEXT REC                       
         CLI   RECORD+3,C' '             4TH CHAR IS BLANK?                     
         BE    MAIN                      YES, GET NEXT MEMBER NAME              
*                                                                               
NXTREC   EQU   *                                                                
         BCT   R3,GETREC                 GET NEXT RECORD                        
*                                                                               
PUTREC   EQU   *                                                                
         MVC   OUTREC+11(10),INNAME      WRITE THE DATA SET NAME                
         PUT   SYSPRINT,OUTREC                                                  
         B     MAIN                      GET NEXT MEMBER NAME                   
*                                                                               
*                                                                               
*************************************************************                   
EXIT     EQU   *                                                                
         CLOSE (SYSPRINT,,ERRPRINT,,INVOUT,,INVIN)                              
         BAS   R14,PCLOS             CLOSE PACB                                 
         L     R15,RTRNCD                                                       
         L     R13,4(R13)                                                       
         ST    R15,16(R13)                                                      
         LM    R14,R12,12(R13)                                                  
         BR    R14                                                              
         EJECT                                                                  
*************************************************************                   
**        SUBROUTINES                                      **                   
*************************************************************                   
POPENSV DC     F'0'                     LINK REG SAVE                           
POPENID DC     CL8'POPEN'               SUBROUTINE ID                           
POPEN    EQU   *                                                                
         ST    R14,POPENSV              SAVE LINK REG                           
   SPACE                                                                        
         XC    ACTION,ACTION            CLEAR RETURN CODE                       
         XC    COMMAREA,COMMAREA        CLEAR GETMAIN ADDRESS                   
         MVI   FUNCTION,C'O'            REQUEST OPEN                            
         CALL  PAM,(PACB,PDDNAME,NOENTRY),VL                                    
         L     R15,ACTION               RETRIEVE RETURN CODE                    
         L     R14,POPENSV              RETRIEVE LINK REG                       
         BR    R14                                                              
   SPACE 2                                                                      
*************************************************************                   
PREADSV  DC    F'0'                     LINK REG SAVE                           
PREADID  DC    CL8'PREAD'               SUBROUTINE ID                           
PREAD    EQU   *                                                                
         ST    R14,PREADSV              SAVE LINK REG                           
   SPACE                                                                        
         XC    ACTION,ACTION            CLEAR RETURN CODE                       
         MVC   COMMENT,NOENTRY          RESET COMMENT PARAMETER                 
         MVI   FUNCTION,C'R'            REQUEST READ                            
         LA    R8,PACNAME                                                       
         CALL  PAM,(PACB,RECORD,(R8),INCLUDES,COMMENT),VL                       
         L     R15,ACTION               RETRIEVE RETURN CODE                    
         L     R14,PREADSV              RETRIEVE LINK REG                       
         BR    R14                                                              
   SPACE 2                                                                      
*************************************************************                   
PCLOSSV  DC    F'0'                     LINK REG SAVE                           
PCLOSID  DC    CL8'PCLOS'               SUBROUTINE ID                           
PCLOS    EQU   *                                                                
         ST    R14,PCLOSSV              SAVE LINK REG                           
   SPACE                                                                        
         XC    ACTION,ACTION            CLEAR RETURN CODE                       
         MVI   FUNCTION,C'C'            REQUEST CLOSE                           
         CALL  PAM,(PACB),VL                                                    
         L     R15,ACTION               RETRIEVE RETURN CODE                    
         L     R14,PCLOSSV              RETRIEVE LINK REG                       
         BR    R14                                                              
   SPACE 2                                                                      
*************************************************************                   
*                                                                               
*   WORK AREAS                                                                  
*                                                                               
*************************************************************                   
SAVE     DC    18F'0'        SAVEAREA AND WORKING STORAGE BASE                  
RTRNCD   DC    F'0'          RETURN CODE                                        
   SPACE                                                                        
INREC    DS    CL80                                                             
         ORG   INREC                                                            
INNAME   DS    CL10                                                             
         ORG                                                                    
OUTREC   DC    CL80'++TRANSFER '                                                
*                                                                               
*                                                                               
BLANKS   DC    CL80' '                                                          
NOENTRY  DC    CL10'NO-ENTRY'                                                   
COMMENT  DC    CL8' '                                                           
INCLUDES DC    CL8'NO-ENTRY'                                                    
RECORD   DC    CL80' '                                                          
PDDNAME  DC    CL8'PANDD1'                                                      
   SPACE                                                                        
         DS    0F                                                               
PACB     DS    0CL12               LENGTH OF A PACB                             
ACTION   DS    F                   PAM RETURN CODE FIELD                        
         DS    XL3                 RESERVED                                     
FUNCTION DS    CL1                 REQUEST CODE:  O=OPEN, S=SEARCH,             
*                                                 R=READ, C=CLOSE               
COMMAREA DS    F                   ADDRESS OF UNIQUE GETMAINED AREA             
*                                    FOR THIS PACB                              
PACNAME  DC    CL10' '               MEMBER NAME FOR THIS PACB                  
PACTRAIL DC    CL2' '                                                           
   SPACE 2                                                                      
PANDCB   DCB   DDNAME=PANDD1,DSORG=PS,MACRF=(RC)                                
INVIN    DCB   DDNAME=INVIN,DSORG=PS,LRECL=80,RECFM=FB,MACRF=GM,       X        
               EODAD=EXIT                                                       
INVOUT   DCB   DDNAME=INVOUT,DSORG=PS,LRECL=80,RECFM=FB,MACRF=PM                
SYSPRINT DCB   DDNAME=SYSPRINT,DSORG=PS,LRECL=80,RECFM=FBA,MACRF=PM             
ERRPRINT DCB   DDNAME=ERRPRINT,DSORG=PS,LRECL=80,RECFM=FBA,MACRF=PM             
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023YYUNJMC   05/01/02'                                      
         END                                                                    
