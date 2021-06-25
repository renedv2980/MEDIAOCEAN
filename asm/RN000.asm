*          DATA SET RN000      AT LEVEL 019 AS OF 09/25/00                      
*PHASE RN000                                                                    
         TITLE 'FIX THE NIELSON NPD TAPE'                                       
RN000    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*RN000**,=V(REGSAVE)                                           
         OPEN  (INPUT,(INPUT),OUTPUT,(OUTPUT))                                  
RDX      PERF  READ                                                             
         CLI   END,C'E'                                                         
         BE    EOJ                                                              
*        AP    CT,=P'1'                                                         
*        CP    CT,=P'51'                                                        
*        BNL   EOJ                                                              
         PERF  FORM                                                             
         PERF  WRITE                                                            
         B     RDX                                                              
EOJ      CLOSE (INPUT,,OUTPUT)                                                  
         XBASE                                                                  
CT       DC    PL2'0'                                                           
READ     NTR1                                                                   
READ2    GET   INPUT,INREC                                                      
         CLI   IFIRST+114,C'1'                                                  
         BNE   READ2                                                            
XIT      XIT1                                                                   
EOF      MVI   END,C'E'                                                         
         B     XIT                                                              
WRITE    NTR1                                                                   
         PUT   OUTPUT,OUTREC                                                    
         B     XIT                                                              
FORM     NTR1                                                                   
         ZAP   CE1,=P'0'                                                        
         MVC   CE2(77),CE1                                                      
         ZAP   CD1,=P'0'                                                        
         MVC   CD2(77),CD1                                                      
         MVC   OFIRST,IFIRST                                                    
         LA    R1,IE1                                                           
         LA    R2,18                                                            
PKE      PACK  0(7,R1),0(7,R1)                                                  
         LA    R1,7(R1)                                                         
         BCT   R2,PKE                                                           
         LA    R1,ID1                                                           
         LA    R2,18                                                            
PKD      PACK  0(7,R1),0(7,R1)                                                  
         LA    R1,7(R1)                                                         
         BCT   R2,PKD                                                           
         SPACE 1                                                                
         AP    CE1,IE1                                                          
         SPACE 1                                                                
         AP    CE2,IE3                                                          
         AP    CE2,IE4                                                          
         AP    CE2,IE5                                                          
         AP    CE2,IE6                                                          
         AP    CE2,IE7                                                          
         AP    CE2,IE8                                                          
         SPACE 1                                                                
         AP    CE3,IE3                                                          
         AP    CE3,IE4                                                          
         SPACE 1                                                                
         AP    CE4,IE3                                                          
         AP    CE4,IE4                                                          
         AP    CE4,IE5                                                          
         SPACE 1                                                                
         AP    CE5,IE4                                                          
         AP    CE5,IE5                                                          
         AP    CE5,IE6                                                          
         SPACE 1                                                                
         AP    CE6,IE11                                                         
         AP    CE6,IE12                                                         
         AP    CE6,IE13                                                         
         AP    CE6,IE14                                                         
         AP    CE6,IE15                                                         
         AP    CE6,IE16                                                         
         SPACE 1                                                                
         AP    CE7,IE11                                                         
         AP    CE7,IE12                                                         
         SPACE 1                                                                
         AP    CE8,IE11                                                         
         AP    CE8,IE12                                                         
         AP    CE8,IE13                                                         
         SPACE 1                                                                
         AP    CE9,IE12                                                         
         AP    CE9,IE13                                                         
         AP    CE9,IE14                                                         
         SPACE 1                                                                
         AP    CE10,IE2                                                         
         AP    CE10,IE10                                                        
         SPACE 1                                                                
         AP    CE11,IE17                                                        
         AP    CE11,IE18                                                        
         SPACE 1                                                                
         AP    CE12,IE18                                                        
         SPACE 3                                                                
         AP    CD1,ID1                                                          
         SPACE 1                                                                
         AP    CD2,ID3                                                          
         AP    CD2,ID4                                                          
         AP    CD2,ID5                                                          
         AP    CD2,ID6                                                          
         AP    CD2,ID7                                                          
         AP    CD2,ID8                                                          
         SPACE 1                                                                
         AP    CD3,ID3                                                          
         AP    CD3,ID4                                                          
         SPACE 1                                                                
         AP    CD4,ID3                                                          
         AP    CD4,ID4                                                          
         AP    CD4,ID5                                                          
         SPACE 1                                                                
         AP    CD5,ID4                                                          
         AP    CD5,ID5                                                          
         AP    CD5,ID6                                                          
         SPACE 1                                                                
         AP    CD6,ID11                                                         
         AP    CD6,ID12                                                         
         AP    CD6,ID13                                                         
         AP    CD6,ID14                                                         
         AP    CD6,ID15                                                         
         AP    CD6,ID16                                                         
         SPACE 1                                                                
         AP    CD7,ID11                                                         
         AP    CD7,ID12                                                         
         SPACE 1                                                                
         AP    CD8,ID11                                                         
         AP    CD8,ID12                                                         
         AP    CD8,ID13                                                         
         SPACE 1                                                                
         AP    CD9,ID12                                                         
         AP    CD9,ID13                                                         
         AP    CD9,ID14                                                         
         SPACE 1                                                                
         AP    CD10,ID2                                                         
         AP    CD10,ID10                                                        
         SPACE 1                                                                
         AP    CD11,ID17                                                        
         AP    CD11,ID18                                                        
         SPACE 1                                                                
         AP    CD12,ID18                                                        
         SPACE 3                                                                
         LA    R1,CE1                                                           
         LA    R2,OE1                                                           
         LA    R3,12                                                            
UPE      AP    0(7,R1),=P'5'         BRING TO 1000'S                            
         MVO   0(7,R1),0(6,R1)                                                  
         AP    0(7,R1),=P'5'                                                    
         MVO   0(7,R1),0(6,R1)                                                  
         AP    0(7,R1),=P'5'                                                    
         MVO   0(7,R1),0(6,R1)                                                  
         UNPK  0(5,R2),0(7,R1)                                                  
         LA    R1,7(R1)                                                         
         LA    R2,5(R2)                                                         
         BCT   R3,UPE                                                           
         SPACE 1                                                                
         LA    R1,CD1                                                           
         LA    R2,OD1                                                           
         LA    R3,12                                                            
UPD      AP    0(7,R1),=P'5'                                                    
         MVO   0(7,R1),0(6,R1)                                                  
         AP    0(7,R1),=P'5'                                                    
         MVO   0(7,R1),0(6,R1)                                                  
*        AP    0(7,R1),=P'5'                                                    
*        MVO   0(7,R1),0(6,R1)                                                  
         UNPK  0(6,R2),0(7,R1)                                                  
         MVC   6(7,R2),=7C' '                                                   
         LA    R1,7(R1)                                                         
         LA    R2,13(R2)                                                        
         BCT   R3,UPD                                                           
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 3                                                                
INREC    DS    0CL427                                                           
IFIRST   DS    CL121                                                            
IE1      DS    CL7                                                              
IE2      DS    CL7                                                              
IE3      DS    CL7                                                              
IE4      DS    CL7                                                              
IE5      DS    CL7                                                              
IE6      DS    CL7                                                              
IE7      DS    CL7                                                              
IE8      DS    CL7                                                              
IE9      DS    CL7                                                              
IE10     DS    CL7                                                              
IE11     DS    CL7                                                              
IE12     DS    CL7                                                              
IE13     DS    CL7                                                              
IE14     DS    CL7                                                              
IE15     DS    CL7                                                              
IE16     DS    CL7                                                              
IE17     DS    CL7                                                              
IE18     DS    CL7                                                              
ID1      DS    CL7                                                              
ID2      DS    CL7                                                              
ID3      DS    CL7                                                              
ID4      DS    CL7                                                              
ID5      DS    CL7                                                              
ID6      DS    CL7                                                              
ID7      DS    CL7                                                              
ID8      DS    CL7                                                              
ID9      DS    CL7                                                              
ID10     DS    CL7                                                              
ID11     DS    CL7                                                              
ID12     DS    CL7                                                              
ID13     DS    CL7                                                              
ID14     DS    CL7                                                              
ID15     DS    CL7                                                              
ID16     DS    CL7                                                              
ID17     DS    CL7                                                              
ID18     DS    CL7                                                              
         DS    CL54                                                             
         SPACE 1                                                                
OUTREC   DS    0CL365                                                           
OFIRST   DS    CL121                                                            
OE1      DS    CL5                                                              
OE2      DS    CL5                                                              
OE3      DS    CL5                                                              
OE4      DS    CL5                                                              
OE5      DS    CL5                                                              
OE6      DS    CL5                                                              
OE7      DS    CL5                                                              
OE8      DS    CL5                                                              
OE9      DS    CL5                                                              
OE10     DS    CL5                                                              
OE11     DS    CL5                                                              
OE12     DS    CL5                                                              
         DS    CL6                                                              
OD1      DS    CL6                                                              
         DS    CL7                                                              
OD2      DS    CL6                                                              
         DS    CL7                                                              
OD3      DS    CL6                                                              
         DS    CL7                                                              
OD4      DS    CL6                                                              
         DS    CL7                                                              
OD5      DS    CL6                                                              
         DS    CL7                                                              
OD6      DS    CL6                                                              
         DS    CL7                                                              
OD7      DS    CL6                                                              
         DS    CL7                                                              
OD8      DS    CL6                                                              
         DS    CL7                                                              
OD9      DS    CL6                                                              
         DS    CL7                                                              
OD10     DS    CL6                                                              
         DS    CL7                                                              
OD11     DS    CL6                                                              
         DS    CL7                                                              
OD12     DS    CL6                                                              
         DS    CL7                                                              
         DS    CL29                                                             
         SPACE 1                                                                
INPUT    DCB   DDNAME=NPDIN,LRECL=427,BLKSIZE=4270,RECFM=FB,DSORG=PS,  X        
               MACRF=GM,EODAD=EOF                                               
OUTPUT   DCB   DDNAME=NPDOUT,LRECL=365,BLKSIZE=3650,RECFM=FB,DSORG=PS, X        
               MACRF=PM                                                         
END      DC    C' '                                                             
         SPACE 3                                                                
CE1      DC    PL7'0'                                                           
CE2      DC    PL7'0'                                                           
CE3      DC    PL7'0'                                                           
CE4      DC    PL7'0'                                                           
CE5      DC    PL7'0'                                                           
CE6      DC    PL7'0'                                                           
CE7      DC    PL7'0'                                                           
CE8      DC    PL7'0'                                                           
CE9      DC    PL7'0'                                                           
CE10     DC    PL7'0'                                                           
CE11     DC    PL7'0'                                                           
CE12     DC    PL7'0'                                                           
         SPACE 1                                                                
CD1      DC    PL7'0'                                                           
CD2      DC    PL7'0'                                                           
CD3      DC    PL7'0'                                                           
CD4      DC    PL7'0'                                                           
CD5      DC    PL7'0'                                                           
CD6      DC    PL7'0'                                                           
CD7      DC    PL7'0'                                                           
CD8      DC    PL7'0'                                                           
CD9      DC    PL7'0'                                                           
CD10     DC    PL7'0'                                                           
CD11     DC    PL7'0'                                                           
CD12     DC    PL7'0'                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019RN000     09/25/00'                                      
         END                                                                    
