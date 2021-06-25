*          DATA SET MONTAXTST  AT LEVEL 039 AS OF 03/20/86                      
*PHASE TAXTEST,*                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE LOADER                                                                 
         TITLE 'TAXTEST - MONTAX TEST FACILITY'                                 
TAXTEST  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TAXTEST,=V(REGSAVE)                                           
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         CLI   FIRST,C'N'                                                       
         BE    GC                                                               
         MVI   FIRST,C'N'                                                       
         MVC   TITLE(24),=C'TAX CALCULATION REQUEST '                           
         MVC   TITLE+24(32),=C'- MONTAX TEST FACILITY (TAXTEST)'                
         MVC   MID1+50(7),=C'ACWDTAX'                                           
         MVC   MID1+60(8),=C'ACWDFICA'                                          
         MVC   MID1+71(8),=C'ACWDUNMP'                                          
         MVC   MID1+82(8),=C'ACWDFICR'                                          
         MVC   MID1(4),=C'RQID'                                                 
         MVC   MID1+5(11),=C'M EX R UNIT'                                       
         MVC   MID1+17(8),=C'CURR SAL'                                          
         MVC   MID1+26(7),=C'YTD SAL'                                           
         MVC   MID1+35(8),=C'YTD FICA'                                          
         MVC   MID1+91(8),=C'CHK DATE'                                          
         MVC   MID1+101(17),=C'FREQ  EMP  DEBTST'                               
         LOAD  EP=MONTAX                                                        
         ST    R0,MONTAX                                                        
GC       GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLI   C,C'/'                                                           
         BE    END                                                              
         MVC   P(4),C                                                           
         MVC   P+5(1),C+4                                                       
         MVC   P+7(2),C+5                                                       
         MVC   P+10(1),C+7                                                      
         MVC   P+12(4),C+8                                                      
         MVC   P+91(8),C+33                                                     
         MVC   P+102(1),C+44                                                    
         MVC   P+107(3),C+41                                                    
         MVC   P+113(4),C+45                                                    
         CLI   C+8,C'C'                                                         
         BE    END                                                              
         PACK  WORK(6),C+12(7)                                                  
         EDIT  (P6,WORK),(8,P+17),2,FLOAT=-                                     
         PACK  WORK(6),C+19(7)                                                  
         EDIT  (P6,WORK),(8,P+26),2,FLOAT=-                                     
         PACK  WORK(6),C+26(7)                                                  
         EDIT  (P6,WORK),(8,P+35),2,FLOAT=-                                     
         GOTO1 MONTAX,DMCB,0,=C'CARDS',C                                        
         EDIT  (P7,C+35),(10,P+47),2,FLOAT=-                                    
         EDIT  (P7,C+42),(10,P+58),2,FLOAT=-                                    
         EDIT  (P7,C+49),(10,P+69),2,FLOAT=-                                    
         EDIT  (P7,C+56),(10,P+80),2,FLOAT=-                                    
         GOTO1 =V(PRINTER)                                                      
         B     GC                                                               
END      GOTO1 =V(PRINTER),DMCB,=C'CLOSE'                                       
         XBASE                                                                  
FIRST    DC    C'Y'                                                             
DMCB     DS    6F                                                               
MONTAX   DS    A                                                                
C        DS    CL80                                                             
DUB      DS    D                                                                
WORK     DS    CL20                                                             
FULL     DS    F                                                                
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039MONTAXTST 03/20/86'                                      
         END                                                                    
