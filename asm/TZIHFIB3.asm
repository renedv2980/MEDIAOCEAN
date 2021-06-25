*          DATA SET TZIHFIB3   AT LEVEL 060 AS OF 09/08/00                      
*PHASE TZIHFIB3                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'TESTFIB -- FIBONACCI SEQUENCE'                                  
***********************************************************************         
*                                                                               
* THE ASSIGNMENT IS TO PRINT THE FIRST 25 FIBONACCI NUMBERS, ONE PER            
* PRINT LINE.  DON'T WORRY ABOUT SUPPRESSING LEADING ZEROES.                    
*                                                                               
* 1.  BE SURE TO CHANGE THE *PHASE CARD ABOVE SO YOU DON'T WIPE                 
*     OUT EACH OTHER'S LOAD MODULES.  REPLACE THE 'XXXX' WITH YOUR              
*     USERID.                                                                   
*                                                                               
* 2.  USE THE CVD AND UNPK INSTRUCTIONS TO PUT EACH BINARY NUMBER               
*     INTO EACH PRINT FIELD.                                                    
*                                                                               
* 3.  THERE IS JCL TO RUN THE PROGRAM IN 'DEIS.DDS.JCL(FIB)' --                 
*     COPY THIS TO YOUR OWN JCL LIBRARY.                                        
*                                                                               
***********************************************************************         
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
* * YOUR CODE HERE *                                                            
MAIN     DS    0H                                                               
         LA    R5,FIBS             R5 POINTS TO ARRAY OF DOUBLEWORDS            
         MVC   0(8,R5),=F'1'       INITIALIZE FIRST NO. TO 1                    
         MVC   0(8,R5),=F'1'       INITIALIZE SECOND NO. TO 1                   
         LA    R5,16(R5)           R5 NOW POINTS TO LOCATION OF 3RD NUM         
*                                                                               
         LA    R2,1                INITIALIZE REGISTERS FOR COMPUTATION         
         LA    R3,1                                                             
         LA    R6,2                R6 IS LOOP COUNTER                           
LOOP     DS    0H                                                               
         LHI   R4,0                INITIALIZE NEXT FIB REGIST TO 0              
         AR    R4,R2               NEXT FIB. NO IS SUM OF PREVIOUS TWO          
         AR    R4,R3                                                            
*                                                                               
         CVD   R4,PACD             CONVERT TO DECIMAL, STORE IN TEMP            
         UNPK  0(8,R5),PACD        UNPACK, STORE IN ARRAY                       
         LR    R2,R3                                                            
         LR    R3,R4               SHIFT NUMBERS                                
         AHI   R5,8                INCREMENT R5 TO POINT TO NEXT DBLWD          
         AHI   R6,1                                                             
         C     R6,QTY                                                           
         BL    LOOP                                                             
*                                                                               
         LA    R6,0                                                             
         LA    R5,FIBS                                                          
         LA    R5,200(R5)                                                       
*LOOP2   DS    0H                                                               
*        MVC   P(8),0(R5)                                                       
*        OC    P+7(8),=X'F0'                                                    
*        L     RF,=V(PRINTER)                                                   
*        BASR  RE,RF                                                            
*        AHI   R5,-8                                                            
*        AHI   R6,1                                                             
*        C     R6,QTY                                                           
*        BL    LOOP                                                             
**********************                                                          
*                                                                               
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
PACD     DS    D                                                                
QTY      DC    F'23'                                                            
FIBS     DS    25D                                                              
DMCB     DS    6F                                                               
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060TZIHFIB3  09/08/00'                                      
         END                                                                    
