*          DATA SET PRDDSTUB   AT LEVEL 004 AS OF 09/14/83                      
*PHASE PRDDSTUB,*                                                               
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
         TITLE 'PRDDSTUB - AYER PAYROLL DIRECT DEPOSIT ADVICE/STUBS'            
         PRINT NOGEN                                                            
PRDDSTUB CSECT                                                                  
         NBASE 0,*DDSTUB*,=V(REGSAVE)                                           
*&&DO                                                                           
         OPEN  DISKIN                                                           
*&&                                                                             
*&&OS                                                                           
         OPEN  (DISKIN,(INPUT))                                                 
*&&                                                                             
         SPACE 2                                                                
RED      GET   DISKIN,DISKAR                                                    
         CLI   DISKAR,C'1'                                                      
         BNE   *+14                                                             
         MVC   ASAC(2),=C'AC'                                                   
         B     *+10                                                             
         MVC   ASAC(2),=C'BL'                                                   
         GOTO1 =V(PRINT),DMCB,DISKAR,ASAC                                       
         B     RED                                                              
*&&DO                                                                           
EOF      CLOSE DISKIN                                                           
*&&                                                                             
*&&OS                                                                           
EOF      CLOSE (DISKIN,)                                                        
*&&                                                                             
         XBASE                                                                  
ASAC     DC    C'BL01'                                                          
DISKAR   DS    0CL133                                                           
         DS    C                                                                
DISKARP1 DS    CL132                                                            
*&&DO                                                                           
DISKIN   DTFSD BLKSIZE=133,DEVADDR=SYS001,RECFORM=FIXUNB,DEVICE=3340,  X        
               WORKA=YES,TYPEFLE=INPUT,EOFADDR=EOF,IOAREA1=DIO                  
*&&                                                                             
*&&OS                                                                           
DISKIN   DCB   DDNAME=DISKIN,          DOS SYS001                      X        
               DSORG=PS,                                               X        
               RECFM=F,                                                X        
               BLKSIZE=00133,          DOS BLKSIZE=00133               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*&&                                                                             
*&&DO                                                                           
DIO      DS    CL133                                                            
*&&                                                                             
DMCB     DS    6F                                                               
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PRDDSTUB  09/14/83'                                      
         END                                                                    
