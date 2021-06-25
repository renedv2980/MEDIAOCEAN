*          DATA SET SPREPFXRP  AT LEVEL 004 AS OF 11/24/98                      
*PHASE SPFX02P                                                                  
         TITLE 'SPFX02 - FIX BAD BDEND DATES'                                   
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
FX10     GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
FX20     SR    R4,R4               TEST IF BUY IS A ROTATOR                     
         IC    R4,BDSEDAY                                                       
         SRDL  R4,4                ISOLATE START DAY IN RE                      
         SRL   R5,28               ISOLATE END DAY IN RF                        
         CR    R4,R5                                                            
         BE    EXIT                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDEND),WORK                                       
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         CR    R0,R5               TEST END DATE/DAY AGREE                      
         BE    EXIT                                                             
         SR    R5,R0               GET NUMBER OF DAYS TO ADD                    
         BP    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R5)                                      
         GOTO1 DATCON,DMCB,WORK+6,(3,BDEND)                                     
*                                                                               
         BAS   RE,PRTBUY                                                        
         GOTO1 PUTBUY                                                           
         B     EXIT                                                             
*                                                                               
FX100    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         MVI   PMED,C'T'                                                        
         CLI   BYTE,X'01'                                                       
         BE    PB10                                                             
         MVI   PMED,C'R'                                                        
         CLI   BYTE,X'02'                                                       
         BE    PB10                                                             
         MVI   PMED,C'N'                                                        
         CLI   BYTE,X'03'                                                       
         BE    PB10                                                             
         MVI   PMED,C'C'                                                        
         CLI   BYTE,X'08'                                                       
         BE    PB10                                                             
         MVI   PMED,C' '                                                        
*                                                                               
PB10     DS    0H                                                               
         LA    R4,PLELDSP                                                       
         GOTO1 HEXOUT,DMCB,BDSEDAY,(R4),1,=C'N'                                 
         MVC   3(6,R4),WORK                                                     
         MVC   10(6,R4),WORK+6                                                  
*                                                                               
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(5),WORK                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY+14,PLDA,4,=C'N'                                  
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PLDA     DS    CL8                                                              
         DS    CL1                                                              
PLELDSP  DS    CL10                                                             
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPFXRP 11/24/98'                                      
         END                                                                    
