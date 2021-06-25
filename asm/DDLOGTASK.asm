*          DATA SET DDLOGTASK  AT LEVEL 003 AS OF 10/26/83                      
*PHASE LOGTASK,*                                                                
         TITLE 'SUPPORT OPERATOR MODIFY COMMAND'                                
         PRINT NOGEN                                                            
LOGTASK  CSECT                                                                  
         NBASE 0,**LGTK**,SAVEREG                                               
         MVC   EXITADDR,0(R1)      SAVE PASSED RTN ADDR                         
         SPACE 1                                                                
* INCREASE THIS TASK'S PRIORITY BY 1 TO RUN ABOVE MAINTASK                      
         SPACE 1                                                                
         CHAP  1,'S'                                                            
         SPACE 3                                                                
*                                                                               
* THIS SUBROUTINE ALLOWS ANY TASK ATTACHING THIS TO ACCEPT MODIFY               
* COMMANDS TO SIMULATE THE DOS STXIT OC FUNCTION.                               
*                                                                               
* 1. THE EXTRACT SVC IS ISSUED TO GET THE COMMUNICATIONS AREA PTR.              
*                                                                               
* 2. THE QEDIT WITH CIBCTR=1 IS ISSUED TO LIMIT MODIFY COMMANDS TO 1            
*    AT A TIME. WHEN 1 MODIFY IS PROCESSED, ANOTHER WILL BE ACCEPTED.           
*                                                                               
* 3. ON OUTPUT REG 4 POINTS TO THE COMMUNICATIONS AREA- SEE                     
*    OS/VS2 SYST PROG LIBRARY - SUPERVISOR PAGES 6 & 7.                         
*                                                                               
*                                                                               
         EXTRACT COMMPTR,'S',FIELDS=(COMM,ASID) GET COMMAREA                    
         SPACE 1                                                                
* REG 4 POINTS TO COMM AREA                                                     
         SPACE 1                                                                
         L     R4,COMMPTR          GET COMM PTR                                 
         USING COMMAREA,R4                                                      
         LA    R8,COMCIBPT         GET ADDR OF CIB PTR FROM COMM AREA           
         L     R9,COMCIBPT         GET CIB ADDR                                 
         LTR   R9,R9               WAS THERE AN ACTIVE CIB                      
         BZ    LOGTASKA            NO                                           
         QEDIT ORIGIN=(R8),BLOCK=(R9) FREE UNWANTED CIB                         
         LTR   RF,RF               DID QEDIT WORK                               
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
LOGTASKA DC    0H'0'                                                            
         QEDIT ORIGIN=(R8),CIBCTR=1 LIMIT OPERATOR TO 1 REQUEST                 
         LTR   RF,RF               DID THAT WORK                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* NOW WAIT ON OPERATOR MODIFY                                                   
*                                                                               
WAIT     DS    0H                                                               
         L     R6,COMECBPT         GET ADDRESS OF COMM ECB                      
         WAIT  ECB=(R6)            WAIT FOR MODIFY COMMAND                      
         L     R3,COMCIBPT         GET ADDRESS OF CIB AFTER POST                
         USING CIBAREA,R3                                                       
         CLI   CIBVERB,CIBMODFY    DETERMINE COMMAND TYPE                       
         BE    MODIFY                                                           
         B     RESET               ONLY MODIFY SUPPORTED (NOT STOP)             
*                                                                               
* WHEN MODIFY FOUND, GO TO RTN ADDRESS FROM ATTACH (ING) PROGRAM                
*                                                                               
MODIFY   DS    0H                                                               
         GOTO1 EXITADDR                                                         
RESET    LA    R8,COMCIBPT         GET ADDRESS OF CIB PTR                       
         L     R9,COMCIBPT         GET ADDRESS OF CIB                           
         QEDIT ORIGIN=(R8),BLOCK=(R9) FREE UP CIB TO ALLOW NEXT                 
         LTR   RF,RF               TEST IF WORKED OK                            
         BZ    WAIT                YES, WAIT ON NEXT OPER ACTION                
         DC    H'0'                                                             
         SPACE 3                                                                
SAVEREG  DC    72D'0'              REGISTER SAVE AREAS                          
EXITADDR DC    A(0)                SAVED EXIT RTN ADDRESS                       
COMMPTR  DC    A(0)                PTR TO COMMUNICATION AREA-EXTRACT            
         DC    A(0)                STORE FOR ASID-EXTRACT                       
COMMAREA DSECT                                                                  
         IEZCOM                                                                 
CIBAREA  DSECT                                                                  
         IEZCIB                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDLOGTASK 10/26/83'                                      
         END                                                                    
