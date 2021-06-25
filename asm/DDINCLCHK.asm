*          DATA SET DDINCLCHK  AT LEVEL 004 AS OF 10/09/15                      
*PHASE INCLCHKA                                                                 
RELO     TITLE 'SEE IF RELO MEMBERS EXIST IN PANDD1.'                           
PANRELO  CSECT                                                                  
         PRINT NOGEN                                                            
         REQUS                                                                  
*        R2                             LOOP CONTROLS                           
*        R3                             PACB BASE                               
*        R4                             UNUSED                                  
*        R5                             UNUSED                                  
*        R6                             UNUSED                                  
*        R7                             WORK                                    
*        R8                             WORK                                    
*        R9                             WORK                                    
*        RA                             WORK - ITERATION COUNTER                
*        RB                             RESERVED                                
*        RC                             PROGRAM BASE                            
*        RD                             WORKING STORAGE BASE                    
*        RE                             SUBROUTINE LINK REG                     
*        RF                             RETURN CODES                            
         SPACE 5                                                                
         USING *,RC                                                             
         STM   RE,RC,12(RD)                                                     
         LA    RC,0(RF)                                                         
         LA    RF,SAVE                                                          
         ST    RF,8(RD)                                                         
         ST    RD,4(RF)                                                         
         LR    RD,RF                                                            
         USING SAVE,RD                                                          
         USING PACB,R3             BASE FOR PACB'S                              
         EJECT                                                                  
*************************************************************                   
*                                                                               
*  OBTAIN AND PROCESS CONTROL CARD.                                             
*                                                                               
*************************************************************                   
CNTL     EQU   *                                                                
         XC    RCODE,RCODE                                                      
         OPEN  (SYSIN,INPUT)                                                    
         OPEN  (SYSPRINT,OUTPUT)                                                
*                                                                               
         MVC   PANDCB+40(8),PDDNAME    INSERT INTO MODEL DCB                    
         RDJFCB PANDCB             GET THE JFCB                                 
         MVC   MSG2DSN,JFCBAREA    EXTRACT THE DATASET NAME                     
*                                                                               
         LOAD  EPLOC=PAM           LOAD PAM MODULE                              
         ST    R0,PAM@             SAVE POINTER TO PAM ENTRY POINT              
         BAS   RE,POPEN                 OPEN PANLIB                             
         LTR   RF,RF                    OPENED?                                 
         BZ    *+16                     YES, OKAY                               
         LHI   RF,12                                                            
         ST    RF,RCODE                                                         
         B     CLOSE                                                            
*                                                                               
         GET   SYSIN,INREC         FIRST CARD IS MEMBER BEING ASSEMBLED         
GETREC   EQU   *                                                                
         GET   SYSIN,INREC               GET THE CONTROL RECORD                 
         MVC   NAME1,BLANKS              ASSURE TRAILING BLANKS                 
         MVC   NAME10,INREC                                                     
         BAS   RE,PSRCH                 FIND MEMBER                             
         LTR   RF,RF                    FOUND ?                                 
         BNZ   GETREC                   NO; GET NEXT INPUT RECORD               
*                                                                               
         MVC   MSG2NM,NAME10                                                    
         MVC   MSGAREA,MSG2                                                     
         PUT   SYSPRINT,PRTREC                                                  
         LHI   RF,4                                                             
         ST    RF,RCODE                                                         
         B     GETREC                                                           
*                                                                               
CLOSE    EQU   *                                                                
         CLOSE (SYSIN,,SYSPRINT)                                                
*                                                                               
*************************************************************                   
EXITX    EQU   *                                                                
         OC    PAM@,PAM@           WAS PAM MODULE LOADED                        
         BZ    EXITZ               NO... DO NOT DELETE MODULE                   
         DELETE EPLOC=PAM          DELETE PAM MODULE                            
         XC    PAM@,PAM@           CLEAR PAM POINTER                            
         SPACE 2                                                                
EXITZ    EQU   *                                                                
         L     RF,RCODE                                                         
         L     RD,4(RD)                                                         
         ST    RF,16(RD)                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  RETURN TO PAN#1                              
         EJECT                                                                  
*************************************************************                   
POPENSV  DC    F'0'                     LINK REG SAVE                           
POPENID  DC    CL8'POPEN'               SUBROUTINE ID                           
POPEN    EQU   *                                                                
         ST    RE,POPENSV               SAVE LINK REG                           
*                                                                               
         LA    R3,PACB1                 GET PACB ADDRESS                        
         XC    ACTION,ACTION            CLEAR RETURN CODE                       
         XC    COMMAREA,COMMAREA        CLEAR GETMAIN ADDRESS                   
         MVI   FUNCTION,C'O'            REQUEST OPEN                            
         L     RF,PAM@                                                          
         CALL  (15),((R3),PDDNAME,NOENTRY),VL                                   
         L     RF,ACTION                RETRIEVE RETURN CODE                    
         L     RE,POPENSV               RETRIEVE LINK REG                       
         BR    RE                                                               
         SPACE 2                                                                
*************************************************************                   
PSRCHSV  DC    F'0'                     LINK REG SAVE                           
PSRCHID  DC    CL8'PSRCH'               SUBROUTINE ID                           
PSRCH    EQU   *                                                                
         ST    RE,PSRCHSV               SAVE LINK REG                           
*                                                                               
         LA    R3,PACB1                 GET PACB ADDRESS                        
         XC    ACTION,ACTION            CLEAR RETURN CODE                       
         MVI   FUNCTION,C'S'            REQUEST SEARCH                          
         MVC   PACTRAIL,BLANKS          ASSURE TRAILING BLANKS                  
         MVC   PACNAME,NAME1            STORE LOCAL TO THIS PACB                
         LA    R8,PACNAME                                                       
         L     RF,PAM@                                                          
         CALL  (15),((R3),DIRECT2,(R8),NOENTRY,NOENTRY,NOENTRY),VL              
         L     RF,ACTION                RETRIEVE RETURN CODE                    
         L     RE,PSRCHSV               RETRIEVE LINK REG                       
         BR    RE                                                               
         SPACE 2                                                                
*************************************************************                   
PCLOSSV  DC    F'0'                     LINK REG SAVE                           
PCLOSID  DC    CL8'PCLOS'               SUBROUTINE ID                           
PCLOS    EQU   *                                                                
         ST    RE,PCLOSSV               SAVE LINK REG                           
*                                                                               
         LA    R3,PACB1                 GET PACB ADDRESS                        
         XC    ACTION,ACTION            CLEAR RETURN CODE                       
         MVI   FUNCTION,C'C'            REQUEST CLOSE                           
         L     RF,PAM@                                                          
         CALL  (15),((R3)),VL                                                   
         L     RF,ACTION                RETRIEVE RETURN CODE                    
         L     RE,PCLOSSV               RETRIEVE LINK REG                       
         BR    RE                                                               
         SPACE 2                                                                
*************************************************************                   
*                                                                               
*   WORK AREAS                                                                  
*                                                                               
SAVE     DC    18F'0'        SAVEAREA AND WORKING STORAGE BASE                  
RCODE    DC    F'0'          RETURN CODE                                        
*                                                                               
PAM@     DC    A(0)                ADDRESS OF PAM ENTRY POINT                   
PAM      DC    CL8'PAM'            PAM MODULE NAME                              
*                                                                               
MVCNAME  MVC   NAME10+2(0),0(R5)                                                
*                                                                               
ASTINCL  DC    CL9'*INCLUDE '                                                   
INREC    DC    CL80' '                                                          
PRTREC   DS    0CL80                                                            
         DC    CL1'0'                                                           
MSGAREA  DC    CL79' '                                                          
*                                                                               
MSG1     DC    CL80' JMCPVMX-01:  SEARCHING FOR '                               
         ORG   MSG1+28                                                          
MSG1NM   DS    CL10                                                             
         ORG                                                                    
MSG2     DC    CL80' JMCPVMX-02:  XXXXXXXXXX WAS FOUND IN'                      
         ORG   MSG2+14                                                          
MSG2NM   DS    CL10                                                             
         ORG   MSG2+38                                                          
MSG2DSN  DS    CL44                                                             
         ORG                                                                    
BLANKS   DC    CL80' '                                                          
*                                                                               
         DS    0F                                                               
NOENTRY  DC    CL10'NO-ENTRY'                                                   
NAME1    DS    0CL22                                                            
NAME10   DS    CL10                                                             
         DS    CL12                                                             
NAME2    DC    CL11'NO-ENTRY   '                                                
COMMENT  DC    CL8' '                                                           
         DC    CL44' '                                                          
SUBSET   DC    CL27' '                                                          
INCLUDES DC    CL8'NO-ENTRY'                                                    
BACKUP   DC    CL8'BACKUP'                                                      
RECORD   DC    CL80' '                                                          
DIRECT2  DS    CL80                                                             
MEMNAME  DC    CL10' '                                                          
PDDNAME  DC    CL8'PANDD1  '                                                    
*                                                                               
PACB1    DS    XL24                 PACB FOR PANDDX                             
*                                                                               
PANDCB   DCB   DDNAME=PANDDNN,DSORG=PS,MACRF=(RC),EXLST=PANELIST                
         DS    0F                                                               
PANELIST DC    X'87',AL3(JFCBAREA)                                              
         DS    0F                                                               
JFCBAREA DC    XL176'00'                                                        
         SPACE 2                                                                
SYSIN    DCB   DDNAME=SYSIN,DSORG=PS,LRECL=80,RECFM=FB,MACRF=GM,       X        
               EODAD=CLOSE                                                      
*                                                                               
SYSPRINT DCB   DDNAME=SYSPRINT,DSORG=PS,LRECL=80,RECFM=FBA,MACRF=PM             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*                                                                               
*     DSECT SECTION                                                             
*                                                                               
*************************************************************                   
PACB     DSECT                                                                  
PACBENT  DS    0CL12               LENGTH OF A PACB                             
ACTION   DS    F                   PAM RETURN CODE FIELD                        
         DS    XL3                 RESERVED                                     
FUNCTION DS    CL1                 REQUEST CODE:  O=OPEN, S=SEARCH,             
*                                                 R=READ, C=CLOSE               
COMMAREA DS    F                   ADDRESS OF UNIQUE GETMAINED AREA             
*                                    FOR THIS PACB                              
PACNAME  DS    CL10                  MEMBER NAME FOR THIS PACB                  
PACTRAIL DS    CL2                                                              
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDINCLCHK 10/09/15'                                      
         END                                                                    
