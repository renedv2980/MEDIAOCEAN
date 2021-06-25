*          DATA SET DDTRCARD   AT LEVEL 053 AS OF 08/26/98                      
*PHASE GCHECARD                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
***********************************************************************         
*                                                                               
*                                                                               
* 1.  BE SURE TO CHANGE THE *PHASE CARD ABOVE SO YOU DON'T WIPE                 
*     OUT EACH OTHER'S LOAD MODULES.  REPLACE THE 'XXXX' WITH YOUR              
*     USERID.                                                                   
*                                                                               
* 2.  USE THE CVD AND UNPK INSTRUCTIONS TO PUT EACH BINARY NUMBER               
*     INTO EACH PRINT FIELD.                                                    
*                                                                               
* 3.  THERE IS JCL TO RUN THE PROGRAM IN 'DEIS.DDS.JCL(XXXXFIB)' --             
*     COPY THIS TO YOUR OWN JCL LIBRARY.                                        
*                                                                               
***********************************************************************         
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
TESTHEX  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TESTHEX,=V(REGSAVE)                                           
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TESTHEX),V(DUMMY)                                              
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
*                                                                               
*                                                                               
LOOP50   DS    0H                                                               
         SR    R7,R7       /CLEANS REGISTER WHERE WE DO CALC.                   
         LA    R4,CARD     /POINTS TO THE BEGINING OF CARD                      
         LA    R6,0                                                             
*BUMP TO THE NEXT ENTRY                                                         
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(6),CARD                                                        
         GOTO1 =V(PRINTER)                                                      
         CLC   CARD(2),=C'/*'  /CHECKS FOR END OF CARD                          
         BE    OUT             /END OF PROGRAM                                  
         B     LOOP                                                             
*                                                                               
*                                                                               
LOOP     DS    0H                                                               
         LA    R3,TABLE        /POINT TO THE TABLE                              
*                                                                               
* MATCHING INPUT WITH CHARACTERS IN TABLE                                       
LOOP05   DS    0H                                                               
         CLC   0(1,R4),SPACES  /END OF ENTRY?                                   
         BE    PRINT                                                            
*                                                                               
*                                                                               
         CLC   0(1,R4),0(R3)   /IF MATCHES DO CALCULATION                       
         BE    LOOP10                                                           
         LA    R3,2(R3)                                                         
         CLI   0(R3),X'FF'     /END OF TABLE?                                   
         BE    INVALID         /IF NO CHAR. WERE FOUND-INV.DATA                 
         B     LOOP05                                                           
*                                                                               
*DOES CALCULATIONS                                                              
*                                                                               
LOOP10   DS    0H                                                               
         ZIC   R6,1(R3)                                                         
         MH    R7,=H'16'                                                        
         AR    R7,R6                                                            
         LA    R4,1(R4)                                                         
         B     LOOP                                                             
*                                                                               
*CONVERT AND PRINT                                                              
*                                                                               
PRINT    DS    0H                                                               
         MVC   P(13),=C'THE INPUT IS:'                                          
         MVC   P+17(L'CARD),CARD                                                
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVC   P(8),=C'OUTPUT: '                                                
         CVD   R7,DUB                                                           
         UNPK  P+12(6),DUB                                                      
         OI    P+17,X'F0'                                                       
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         B     LOOP50                                                           
*                                                                               
*                                                                               
INVALID  MVC   P(13),=C'INVALID DATA'                                           
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         B     LOOP50                                                           
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
OUT      XBASE                                                                  
         EJECT                                                                  
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
DUB      DS    D                                                                
TAB      DC    C'JAN',P'031',P'031'                                             
         DC    C'FEB',P'059',P'060'                                             
         DC    C'MAR',P'090',P'091'                                             
         DC    C'APR',P'120',P'121'                                             
         DC    C'MAY',P'151',P'152'                                             
         DC    C'JUN',P'181',P'182'                                             
         DC    C'JUL',P'212',P'213'                                             
         DC    C'AUG',P'243',P'244'                                             
         DC    C'SEP',P'273',P'274'                                             
         DC    C'OCT',P'304',P'305'                                             
         DC    C'NOV',P'334',P'335'                                             
         DC    C'DEC',P'365',P'366'                                             
         DC    X'FF'                                                            
DAYREC   DSECT                                                                  
MONTH    DS    CL3                                                              
NLEAP    DS    XL3                                                              
LEAP     DS    XL3                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053DDTRCARD  08/26/98'                                      
         END                                                                    
