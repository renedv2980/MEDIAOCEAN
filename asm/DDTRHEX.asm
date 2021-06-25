*          DATA SET DDTRHEX    AT LEVEL 037 AS OF 08/21/98                      
*PHASE GCHEHEX                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
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
         LA    R4,INPUT                                                         
         LA    R6,0                                                             
*                                                                               
*                                                                               
LOOP     DS    0H                                                               
         LA    R3,TABLE                                                         
*                                                                               
* MATCHING INPUT WITH CHARACTERS IN TABLE                                       
LOOP05   DS    0H                                                               
         CLC   0(1,R4),SPACES                                                   
         BE    PRINT                                                            
         CLC   0(1,R4),0(R3)     /                                              
         BE    LOOP10                                                           
         LA    R3,2(R3)                                            .            
         CLI   0(R3),X'FF'                                                      
         BE    INVALID                                                          
         B     LOOP05                                                           
*                                                                               
LOOP10   DS    0H                                                               
         ZIC   R6,1(R3)                                                         
         MH    R7,=H'16'                                                        
         AR    R7,R6                                                            
         LA    R4,1(R4)                                                         
         B     LOOP                                                             
*                                                                               
*                                                                               
PRINT    DS    0H                                                               
         MVC   P(13),=C'THE INPUT IS:'                                          
         MVC   P+17(L'INPUT),INPUT                                              
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVC   P(8),=C'OUTPUT: '                                                
         CVD   R7,DUB                                                           
         UNPK  P+12(6),DUB                                                      
         OI    P+17,X'F0'                                                       
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         B     END                                                              
*                                                                               
INVALID  MVC   P(13),=C'INVALID DATA'                                           
         L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         B     END                                                              
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
END      XBASE                                                                  
         EJECT                                                                  
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
DMCB     DS    6F                                                               
INPUT    DC    C'3A42 '                                                         
DUB      DS    D                                                                
TABLE    DC    C'0',X'00'                                                       
         DC    C'1',X'01'                                                       
         DC    C'2',X'02'                                                       
         DC    C'3',X'03'                                                       
         DC    C'4',X'04'                                                       
         DC    C'5',X'05'                                                       
         DC    C'6',X'06'                                                       
         DC    C'7',X'07'                                                       
         DC    C'8',X'08'                                                       
         DC    C'9',X'09'                                                       
         DC    C'A',X'0A'                                                       
         DC    C'B',X'0B'                                                       
         DC    C'C',X'0C'                                                       
         DC    C'D',X'0D'                                                       
         DC    C'E',X'0E'                                                       
         DC    C'F',X'0F'                                                       
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037DDTRHEX   08/21/98'                                      
         END                                                                    
