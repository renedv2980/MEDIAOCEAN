*          DATA SET DEISIDFL   AT LEVEL 003 AS OF 05/01/02                      
*PHASE DEISIDFL                                                                 
         TITLE 'TEST IDF'                                                       
DEISIDFL CSECT                                                                  
*                                                                               
* THIS CODE IS OUR STANDARD INITIALIZATION STUFF                                
*                                                                               
         STM   RE,RC,12(RD)                                                     
         BASR  RB,0                                                             
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2(0)                                                           
         DC    CL8'IDFTEST'                                                     
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
         L     RC,=A(REGSAVE)                                                   
         CNOP  0,4                                                              
         B     *+8                                                              
         DC    A(*)                                                             
         LA    RF,*-4                                                           
         S     RF,*-8                                                           
         AR    RC,RF                                                            
         SR    RF,RF                                                            
         ICM   RF,3,20(RB)                                                      
         LA    RF,16(RC,RF)                                                     
         ST    RF,8(RD)                                                         
         ST    RD,4(RF)                                                         
         LR    RD,RF                                                            
         MVC   0(4,RC),24(RB)                                                   
         XC    4(4,RC),4(RC)                                                    
         ST    RD,8(RC)                                                         
         ST    R1,12(RC)                                                        
         LA    RC,16(RC)                                                        
         L     RE,4(RD)                                                         
         LM    RE,R1,12(RE)                                                     
*                                                                               
* HERE COMES THE APPLICATION CODE                                               
*                                                                               
         NOP   *                                                                
*                                                                               
         MVC   A,=C'ABCDEFGHIJ'                                                 
         MVC   B,=C'KLMNOPQRST'                                                 
         LHI   R1,5                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   A(0),B                                                           
         NOP   *                                                                
*                                                                               
         MVC   A,=C'ABCDEFGHIJ'                                                 
         MVC   B,=C'KLMNOPQRST'                                                 
         LHI   R1,5                                                             
         MVC   A(0),B                                                           
         EX    R1,*-6                                                           
*                                                                               
         LHI   R5,10                                                            
         NOP   *                                                                
*                                                                               
* EXIT THE PROGRAM                                                              
*                                                                               
         SR    RF,RF                                                            
         L     RD,4(RD)                                                         
         L     RE,12(RD)                                                        
         LM    R0,RC,20(RD)                                                     
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
A        DS    CL10                                                             
B        DS    CL10                                                             
         SPACE 3                                                                
REGSAVE  DS    500D                REGISTER SAVE AREA                           
         SPACE 3                                                                
R0       EQU   0                                                                
R1       EQU   1                                                                
R2       EQU   2                                                                
R3       EQU   3                                                                
R4       EQU   4                                                                
R5       EQU   5                                                                
R6       EQU   6                                                                
R7       EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                                                                
RA       EQU   10                                                               
RB       EQU   11                                                               
RC       EQU   12                                                               
RD       EQU   13                                                               
RE       EQU   14                                                               
RF       EQU   15                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DEISIDFL  05/01/02'                                      
         END                                                                    
