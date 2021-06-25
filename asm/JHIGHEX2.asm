*          DATA SET JHIGHEX2   AT LEVEL 184 AS OF 09/19/00                      
*PHASE JHIGHEX2                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE STXITER                                                                
         TITLE 'JHIGHEX2 -- HEX TO DECIMAL'                                     
*                                                                               
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
TESTFIB  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TESTFIB,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TESTFIB),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* PUT YOUR CODE HERE *                                                          
**********************                                                          
*                                                                               
COUNTER  EQU   17                                                               
LIMIT    EQU   8                                                                
         LA    R2,COUNTER       TO BE USED AS COUNTER FOR MYTABLE               
         LA    R3,0             COUNTER KEEPS TRACK OF LENGTH OF INPUT          
         LA    R5,MYTABLE       USED TO STEP THOUGH MYTABLE                     
         SR    R6,R6            CLEAR R6 TO USE TO STORE NUMBER                 
         SR    R7,R7                                                            
         MVC   P+3(18),=C'HEXADECIMAL NUMBER'                                   
         MVC   P+33(14),=C'DECIMAL NUMBER'                                      
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVC   P+3(18),=C'------------------'                                   
         MVC   P+33(14),=C'--------------'                                      
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         USING HEXDEC,R5                                                        
*                                                                               
MYLOOP   GOTO1 =V(CARDS),DMCB,IOAREA,=C'RE00'                                   
         CLC   IOAREA(2),=C'/*'                                                 
         BE    DONE                                                             
         LA    R4,IOAREA                                                        
         MVC   P+9(8),0(R4)                                                     
*                                                                               
VERIFY   CLC   0(1,R4),CHAR     CHECK TO SEE IF CHARACTERS ARE VALID            
         BNE   NEXT                                                             
         CLI   0(R4),C' '       CHECK FOR SPACE                                 
         BE    PRINTDEC                                                         
         MVC   BYTE,0(R4)       PUT ONE BYTE INTO STORAGE                       
         TM    BYTE,X'F0'       CHECK TO SEE IF IT IS NUMBER                    
         BO    NUMBER                                                           
         ZIC   R9,BYTE          CONVERT TO HEX NUMBER                           
         SHI   R9,X'C0'                                                         
         AHI   R9,9                                                             
         B     *+14                                                             
*                                                                               
NUMBER   ZIC   R9,BYTE          CONVERT TO HEX NUMBER                           
         SHI   R9,X'F0'                                                         
         MHI   R6,16            MULTIPLY BY 16                                  
         AR    R6,R9            ADD NEXT NUMBER                                 
         AHI   R3,1                                                             
         AHI   R4,1                                                             
         LA    R5,MYTABLE       REINIT CHAR TABLE                               
         LA    R2,COUNTER       REINIT COUNTER                                  
         CHI   R3,LIMIT         CHECK IF STRING IS AT LENGTH LIMIT              
         BE    PRINTDEC                                                         
         B     VERIFY                                                           
*                                                                               
NEXT     AHI   R5,1             COMPARE TO NEXT CHAR IN TABLE                   
         BCT   R2,VERIFY        INVALID CHARACTER ENTERED                       
*                                                                               
INVALID  MVC   P+28(25),=C'INVALID CHARACTER ENTERED'                           
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         SR    R6,R6                                                            
         SR    R3,R3                                                            
         LA    R2,COUNTER                                                       
         LA    R5,MYTABLE                                                       
         B     MYLOOP                                                           
*                                                                               
PRINTDEC DS    0H                                                               
         EDIT  (R6),(10,P+35),ZERO=BLANK                                        
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         LA    R5,MYTABLE                                                       
         SR    R6,R6                                                            
         SR    R3,R3                                                            
         LA    R2,COUNTER                                                       
         B     MYLOOP           GO BACK TO TOP OF LOOP                          
*                                                                               
DONE     DS    0H                                                               
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
BYTE     DS    X                                                                
WORK     DS    CL64                                                             
IOAREA   DS    CL80                                                             
OUTP     DS    D                                                                
MYTABLE  DC    C'F'                                                             
         DC    C'E'                                                             
         DC    C'D'                                                             
         DC    C'C'                                                             
         DC    C'B'                                                             
         DC    C'A'                                                             
         DC    C'9'                                                             
         DC    C'8'                                                             
         DC    C'7'                                                             
         DC    C'6'                                                             
         DC    C'5'                                                             
         DC    C'4'                                                             
         DC    C'3'                                                             
         DC    C'2'                                                             
         DC    C'1'                                                             
         DC    C'0'                                                             
         DC    C' '                                                             
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
       ++INCLUDE JHIGDSEC                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'184JHIGHEX2  09/19/00'                                      
         END                                                                    
