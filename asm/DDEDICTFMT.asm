*          DATA SET DDEDICTFMT AT LEVEL 004 AS OF 08/13/00                      
*PHASE EDICTFMA EDICTFMT                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'EDICTFMT - EDICT TRANSACTION FILE FORMATTER'                    
EDICTFMT CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,EDICTFMT,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     R9,=V(CPRINT)       SET FOR REPORT                               
         USING DPRINT,R9                                                        
         MVC   TITLE(32),=C'EDICT TRANSACTION FILE FORMATTER'                   
         EJECT                                                                  
         GOTO1 =V(DADDS),DMCB,DAOPEN,BLOCK,0,EDICTFL,0,0                        
         LA    R2,EDICTFL                                                       
         MVC   DNEXT-DTFPHD(4,R2),=X'00010000'                                  
*                                                                               
         GOTO1 =V(DADDS),DMCB,DARPT,,F'18432'                                   
         LH    R5,DMCB+10          RECORDS (BLOCKS) PER TRACK                   
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   P(29),=C'RECORDS (BLOCKS) PER TRACK = '                          
         EDIT  (R5),(3,P+29),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LR    R1,R5               DYNAMICALLY BUILD CCW CHAIN                  
         BCTR  R1,0                                                             
         LA    R2,BUF                                                           
DYNCCW   MVC   12(12,R2),0(R2)                                                  
         LA    R2,12(R2)                                                        
         BCT   R1,DYNCCW                                                        
*                                                                               
         LA    R2,12(R2)           COMPUTE LENGTH OF CCW CHAIN                  
         S     R2,=A(BUF)                                                       
*                                                                               
         SR    R3,R3               WRITE TRACKS UNTIL END OF FILE               
LOOP     GOTO1 =V(DADDS),DMCB,WTTRK,CCW,(R2),EDICTFL,ADDR,(X'FF',BUF)           
         TM    9(R1),X'04'                                                      
         BO    EOF                                                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AR    R3,R5               COUNT RECORDS                                
         B     LOOP                                                             
*                                                                               
EOF      MH    R3,=H'72'           LOGICAL RECORDS PER BLOCK                    
         SR    R2,R2                                                            
         D     R2,=F'31'           DAYS PER MONTH                               
         MVC   P(34),=C'NUMBER OF REPORTS/DAY SUPPORTED = '                     
         EDIT  (R3),(7,P+34),ALIGN=LEFT                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
EDICTFL  DMDA                                                                   
         SPACE 3                                                                
DUB      DS    D                                                                
DMCB     DS    8F                                                               
ADDR     DS    F                                                                
*                                                                               
UTL      DC    F'0',F'0'                                                        
SSB      DC    F'0'                                                             
*                                                                               
         DS    0D                                                               
CCW      DS    256X                                                             
WORK     DS    CL17                                                             
*                                                                               
         DS    0D                                                               
BUF      DC    6X'00'              COUNT FIELD                                  
         DC    AL2(18432)                                                       
         DC    A(BLOCK)            ADDRESS OF DATA AREA                         
         DS    200X                                                             
LBUF     DS    F                                                                
BLOCK    DC    60000X'00'                                                       
         EJECT                                                                  
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDEDICTFMT08/13/00'                                      
         END                                                                    
