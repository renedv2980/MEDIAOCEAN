*          DATA SET TZIHDATA   AT LEVEL 048 AS OF 09/15/00                      
*PHASE TZIHDATA                                                                 
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
MAIN      DS    0H                                                              
          GOTO1 =V(CARDS),DMCB,IO,=C'RE00'                                      
          LA    R2,IO               POINT R2 TO BEGINNING OF IN DATA            
          DC    H'0'                                                            
*                                                                               
          LA    R5,4                                                            
MLOOP     BAS   RE,READPAIR                                                     
          MVC   P(10),WORK                                                      
          L     RF,=V(PRINTER)                                                  
          BASR  RE,RF                                                           
          AHI   R2,1                                                            
          BCT   R5,MLOOP                                                        
**********************                                                          
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
* SUBS    GO    HERE                                                            
*                                                                               
READPAIR  DS    0H                  GETS KEYWORD AND VALUE                      
*                                                                               
LOOP      DS    0H                                                              
          LA    R3,WORK                                                         
          CLI   0(R2),C' '                                                      
          BE    OVER                                                            
          CLI   0(R2),C','                                                      
          BE    OVER                                                            
          MVC   0(1,R3),0(R2)                                                   
          AHI   R2,1                                                            
          AHI   R3,1                                                            
          B     LOOP                                                            
OVER      DS    0H                  R2 POINTS AT COMMA                          
          BR    RE                                                              
*                                                                               
DUB       DS    D                                                               
DMCB      DS    6F                                                              
INPUT     DS    4F                                                              
FULL      DS    F                                                               
HALF      DS    H                                                               
IO        DS    CL80                                                            
WORK      DS    CL64                                                            
KEYWD     DS    CL5                                                             
VALUE     DS    CL4                                                             
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
IND       DSECT                                                                 
Y1        DS    F                                                               
D1        DS    F                                                               
Y2        DS    F                                                               
D2        DS    F                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048TZIHDATA  09/15/00'                                      
         END                                                                    
