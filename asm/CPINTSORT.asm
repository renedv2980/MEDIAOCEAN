*          DATA SET CPINTSORT  AT LEVEL 065 AS OF 03/12/85                      
*PHASE CPINFIX,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
         PRINT NOGEN                                                            
CPINFIX  CSECT                                                                  
         NBASE 500,CPINFIX,=V(REGSAVE)                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVI   SECOND,0                                                         
         OPEN  (IN1,(INPUT))                                                    
         OPEN  (OUT,(OUTPUT))                                                   
GET      GET   IN1,INREC                                                        
         CLI   INREC,C'A'                                                       
         BNE   GET2                                                             
         CLI   SECOND,1                                                         
         BE    ENDJOB                                                           
GET2     MVI   SECOND,1                                                         
         L     RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         ST    RE,COUNT                                                         
         PUT   OUT,INREC                                                        
         B     GET                                                              
         SPACE 2                                                                
ENDJOB   CLOSE (IN1)                                                            
         CLOSE (OUT)                                                            
         MVC   P(14),=C'INPUT RECORDS '                                         
         EDIT  (P4,COUNT),(6,P+15)                                              
         XBASE                                                                  
         LTORG                                                                  
COUNT    DC    F'0'                                                             
SECOND   DC    X'00'                                                            
DUB      DS    D                                                                
WORK     DS    CL17                                                             
INREC    DS    CL40                                                             
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00040,                                            X        
               BLKSIZE=04000,                                          X        
               EODAD=ENDJOB,                                           X        
               MACRF=GM                                                         
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00040,                                            X        
               BLKSIZE=04000,                                          X        
               MACRF=PM                                                         
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065CPINTSORT 03/12/85'                                      
         END                                                                    
