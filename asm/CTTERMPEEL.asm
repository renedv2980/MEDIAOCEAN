*          DATA SET CTTERMPEEL AT LEVEL 006 AS OF 08/17/00                      
*PHASE CTPEELA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE DUMPOUT                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'TERMINAL RECORD COPY PROGRAM'                                   
CTPEEL   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,CTPEEL,=V(REGSAVE),RA                                          
*                                                                               
         XC    DUB,DUB                                                          
         ST    RB,DUB                                                           
         L     R2,=V(STXITER)                                                   
         ST    R2,DUB+4                                                         
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8           R8=A(PRINT CSECT)                            
         MVC   TITLE(19),=C'INPUT CONTROL CARDS'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OPEN  (TINT,INPUT)                                                     
         OPEN  (TOUT,OUTPUT)                                                    
         SR    R3,R3                                                            
         EJECT                                                                  
NEXTREC  GET   TINT,IO             GET ALL RECORDS BEFORE 'T' RECORDS           
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
*                                                                               
         CLI   IO+4,C'T'           TEST TERMINAL RECORD                         
         BE    TERM1               YES                                          
*                                                                               
         CH    R3,=H'5'            PEEL 5 RECORDS                               
         BE    NEXTREC                                                          
*                                                                               
         LA    R3,1(R3)                                                         
         PUT   TOUT,IO             WRITE OUT THE RECORD                         
         L     R1,NUMOUT                                                        
         LA    R1,1(R1)            INCREMENT OUTPUT FILE COUNTER                
         ST    R1,NUMOUT                                                        
         B     NEXTREC                                                          
         EJECT                                                                  
TERM1    SR    R3,R3                                                            
         B     TERM2                                                            
*                                                                               
TERM3    GET   TINT,IO             GET PASSIVE 'T' RECORDS                      
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
*                                                                               
         CLI   IO+4,C'T'           TEST TERMINAL RECORD                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         OC    IO+5(22),IO+5                                                    
         BNZ   TERM4                                                            
*                                                                               
         CH    R3,=H'5'            PEEL 5 RECORDS                               
         BE    TERM3                                                            
*                                                                               
TERM2    LA    R3,1(R3)                                                         
         PUT   TOUT,IO             WRITE OUT THE RECORD                         
         L     R1,NUMOUT                                                        
         LA    R1,1(R1)            INCREMENT OUTPUT FILE COUNTER                
         ST    R1,NUMOUT                                                        
         B     TERM3                                                            
         EJECT                                                                  
TERM4    SR    R3,R3                                                            
         SR    R4,R4                                                            
         B     TERM6                                                            
*                                                                               
TERM5    GET   TINT,IO             GET 'T' RECORDS                              
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
*                                                                               
         CLI   IO+4,C'T'           TEST TERMINAL RECORD                         
         BNE   TERM10              NO                                           
*                                                                               
         CH    R4,=H'15'                                                        
         BE    TERM5                                                            
*                                                                               
         CH    R3,=H'3'            PEEL 3 RECORDS                               
         BE    TERM20                                                           
*                                                                               
TERM6    LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         PUT   TOUT,IO             WRITE OUT THE RECORD                         
         L     R1,NUMOUT                                                        
         LA    R1,1(R1)            INCREMENT OUTPUT FILE COUNTER                
         ST    R1,NUMOUT                                                        
         B     TERM5                                                            
*                                                                               
TERM20   LA    R6,100                                                           
TERM21   GET   TINT,IO             GET 'T' RECORDS                              
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
         BCT   R6,TERM21                                                        
         SR    R3,R3                                                            
         B     TERM5                                                            
*                                                                               
         EJECT                                                                  
TERM10   SR    R3,R3                                                            
*                                                                               
TERM11   PUT   TOUT,IO             WRITE OUT THE RECORD                         
         L     R1,NUMOUT                                                        
         LA    R1,1(R1)            INCREMENT OUTPUT FILE COUNTER                
         ST    R1,NUMOUT                                                        
*                                                                               
         LA    R3,1(R3)                                                         
         CH    R3,=H'5'                                                         
         BE    WRAP30                                                           
*                                                                               
         GET   TINT,IO             GET ALL REMAINING RECORDS                    
         L     R1,NUMIN                                                         
         LA    R1,1(R1)            INCREMENT INPUT FILE COUNTER                 
         ST    R1,NUMIN                                                         
         B     TERM11                                                           
         EJECT                                                                  
WRAP30   MVC   P(27),=C'NUMBER OF INPUT RECORDS:   '                            
         EDIT  NUMIN,(8,P+37)                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(27),=C'NUMBER OF OUTPUT RECORDS:  '                            
         EDIT  NUMOUT,(8,P+37)                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(27),=C'NUMBER OF ADDED RECORDS:   '                            
         EDIT  NUMADD,(8,P+37)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EXIT     CLOSE (TINT,)                                                          
         CLOSE (TOUT,)                                                          
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
TINT     DCB   DDNAME=TINT,DSORG=PS,MACRF=(GM),EODAD=WRAP30,           *        
               RECFM=VB,BLKSIZE=8200,LRECL=2048,BUFNO=2                         
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,BLKSIZE=8200,LRECL=2048,BUFNO=2                         
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
NUMIN    DC    F'0'                                                             
NUMOUT   DC    F'0'                                                             
NUMADD   DC    F'0'                                                             
WORK     DS    XL17                                                             
*                                                                               
         DC    C'***IO***'                                                      
IO       DS    CL8000                                                           
         SPACE 3                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTTERMPEEL08/17/00'                                      
         END                                                                    
